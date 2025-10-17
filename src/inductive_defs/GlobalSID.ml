(* Global system of inductive definitions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open ID_sig
open SID.ID
open MemoryModel

let sid_original = ref SID.empty
let sid_updated = ref SID.empty

let cache = ref PredicateAbstraction.M.empty

let show () =
  Format.asprintf "Original:\n%s\nUpdated:%s\n" (SID.show !sid_original) (SID.show !sid_updated)

let reset () =
  sid_original := SID.empty;
  sid_updated := SID.empty

let reset_results () = cache := PredicateAbstraction.M.empty

let compute_graph () =
  sid_original := SID.compute_graph !sid_original;
  sid_updated := SID.compute_graph !sid_updated

let select original = if original then !sid_original else !sid_updated

let register_builtin id =
  sid_original := SID.register_builtin !sid_original id;
  sid_updated := SID.register_builtin !sid_updated id

let register_user_defined id =
  sid_original := SID.register_user_defined !sid_original id;
  sid_updated := SID.register_user_defined !sid_updated id

let update_user_defined id =
  sid_original := SID.update_user_defined !sid_original id;
  sid_updated := SID.update_user_defined !sid_updated id

let is_builtin pred = SID.is_builtin !sid_original pred
let is_user_defined pred = SID.is_user_defined !sid_original pred

let find ?(original=false) = SID.find (select original)
let find_user_defined ?(original=false) = SID.find_user_defined (select original)
let fold_user_defined ?(original=false) fn = SID.fold_user_defined fn (select original)

let get_user_defined ?(original=false) () = SID.get_user_defined (select original)

let is_self_recursive pred = SID.is_self_recursive !sid_updated pred

let dependencies ?(original=false) = SID.dependencies (select original)

let dependency_graph () = SID.dependency_graph !sid_updated

let get () = !sid_updated

let unfold name xs = SID.unfold !sid_updated name xs

(** ==== Context ==== *)

module S = Stdlib.Set.Make(String)
module M = Stdlib.Map.Make(String)

let builtin_sorts struct_defs =
  M.bindings struct_defs
  |> List.map snd
  |> BatList.concat_map (StructDef.get_sorts)
  |> List.fold_left (fun acc s ->
      let all_names = Sort.all_names s in
      List.fold_left (fun acc name -> M.add name s acc) acc all_names
    ) M.empty

let builtin_structs () =
  SID.fold_builtin (fun (module B : BUILTIN) acc -> acc @ B.struct_defs) !sid_original []
  |> List.fold_left (fun acc s -> M.add (StructDef.show_cons s) s acc) M.empty

let builtin_heap_sort () =
  SID.fold_builtin (fun (module B : BUILTIN) acc -> B.heap_sort :: acc) !sid_original []
  |> HeapSort.union

let builtin_ids () =
  SID.get_builtin !sid_original
  |> List.map (fun (module B : BUILTIN) -> B.name)
  |> S.of_list

let builtin_context () =
  let struct_defs = builtin_structs () in
  let sorts = builtin_sorts struct_defs in
  let heap_sort = builtin_heap_sort () in
  let ids = builtin_ids () in
  ParserContext.empty ~sorts ~struct_defs ~heap_sort ~ids ()

(** ==== Preprocessing ==== *)

(** Apply the function to each inductive definition *)
let preprocess_user_definitions fn =
  sid_updated :=
    SID.filter_map (fun id -> match id with
      | Builtin _ -> Some id
      | UserDefined id -> match fn id with
        | None -> None
        | Some id -> Some (UserDefined id)
    ) !sid_updated

(** ==== Syntactic queries ==== *)

let get_structs visited get_continue name = match find name with
  | Builtin (module B : BUILTIN) -> B.struct_defs
  | UserDefined id ->
    if BatList.mem_cmp String.compare name visited then []
    else get_continue (name::visited) @@ InductiveDefinition.instantiate_formals id

(** ==== Semantics queries ==== *)

let has_unique_footprint name = match find name with
  | Builtin (module B : BUILTIN) -> B.unique_footprint
  | UserDefined id -> failwith "TODO: SID.unique_fp"

(** ==== BUILTINS: General ==== *)

let instantiate heap_sort name operands = match find name with
  | Builtin (module B : BUILTIN) -> B.instantiate heap_sort operands
  | UserDefined id -> Result.Ok (SL.mk_predicate name operands)


(** ==== BUILTINS: Model checking ==== *)

let model_check name instance sh = match find name with
  | Builtin (module B : BUILTIN) -> B.model_check instance sh
  | UserDefined _ -> failwith "TODO: model check UID"

let compute_footprints name instance sh = match find name with
  | Builtin (module B : BUILTIN) -> B.compute_footprints instance sh
  | UserDefined _ -> failwith "TODO: compute_footprints UID"

(** ==== BUILTINS: Translation ==== *)

module Translation (E : Translation_sig.ENCODING) = struct

  let translate name (context : E.Context.t) instance domain sxs = match find name with
    | Builtin (module B : BUILTIN) ->
      let module T = B.Translation(E) in
      let bound =
        B.Bound.compute context.sl_graph context.phi instance context.location_bounds in
      T.translate context instance domain sxs bound
    | UserDefined id ->
      let reason =
        Format.asprintf "User-defined inductive predicate %s is not unfolded before translation to SMT"
          (id.name)
      in
      Exceptions.internal_error ~reason ~details:(InductiveDefinition.show id)
end


(** ==== Abstraction of predicates ==== *)

let is_computed () = not @@ PredicateAbstraction.M.is_empty !cache

let abstraction name =  match find name with
  | UserDefined id -> PredicateAbstraction.M.find id !cache

(* TODO: check whether we really compute what we want! *)
let rec existentials ?(visited=[]) id =
  if BatList.mem_cmp InductiveDefinition.compare id visited then []
  else
    let unfolding = InductiveDefinition.instantiate_formals ~refresh:false id in
    let rec_calls =
      SL.select_subformulae SL.is_predicate unfolding
      |> List.map SL.as_predicate
      |> List.map fst
    in
    SL.bound_vars unfolding @ List.concat_map (existentials ~visited:(id::visited)) (List.map find_user_defined rec_calls)

(** ==== Location bound computation ==== *)

let compute_aux phi g id x a =
  let open InductiveDefinition in
  let open PredicateAbstraction in
  if SL.is_symbolic_heap phi then (Float.of_int a.stable_size)
  else
  let lhs, _ = SL.as_entailment phi in
  let _, atoms = SL.as_symbolic_heap lhs in
  let c = false && List.for_all (fun atom -> match SL.view atom with
    | Predicate (name, y :: ys, _) when String.equal name id.name ->
      if SL.Term.equal x y then (* TODO *)
        List.for_all (SL_graph0.must_neq g SL.Term.nil) ys
      else true
    | _ -> true
  ) atoms
  in
  if c then (Float.of_int a.stable_size) else a.fixpoint_size

let term_bound phi g heap_sort x =
  let sort = SL.Term.get_sort x in
  SID.fold (fun pred acc ->
    let bound = match pred with
    | Builtin (module B : BUILTIN) -> B.term_bound phi heap_sort x
    | UserDefined id ->
      let abstraction = PredicateAbstraction.M.find id !cache in
      if Sort.equal (SL.Variable.get_sort abstraction.root) sort then compute_aux phi g id x abstraction
      else Float.one
    in
    max acc bound
  ) !sid_updated Float.one

let alloc name phi g heap_sort xs = match find name with
  | UserDefined id -> Float.of_int (PredicateAbstraction.M.find id !cache).unfolding_depth
  | Builtin (module B : BUILTIN) ->
    B.must_allocated xs
    |> List.map (term_bound phi g heap_sort)
    |> BatList.kahan_sum

let additional_bounds phi =
  SID.fold (fun pred acc -> match pred with
    | Builtin (module B : BUILTIN) -> LocationBounds0.plus acc @@ B.additional_bound phi
    | UserDefined id -> acc
  ) !sid_updated LocationBounds0.empty

(** TODO: compute some must-relations *)
let sl_graph name instance = match find name with
  | Builtin (module B : BUILTIN) -> B.sl_graph instance
  | UserDefined id -> SL_graph0.empty

let unfolding_depth phi g name xs = match find name with
  | UserDefined id -> (PredicateAbstraction.M.find id !cache).unfolding_depth
  (*  let abstraction = PredicateAbstraction.find id !cache in
    compute_aux phi g id (List.hd xs) abstraction
  *)

let formula_preprocessing phi =
  SID.fold (fun pred acc -> match pred with
    | Builtin (module B : BUILTIN) -> B.global_preprocessing phi
    | UserDefined id -> acc
  ) !sid_updated phi

let formula_preprocessing_ctx ctx = Context.{ctx with phi = formula_preprocessing ctx.phi}
