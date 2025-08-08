open ID
open ID_sig

module L = Logger.Make
  (struct let name = "SID" let level = 1 end)

include SID0

module Logger = L

let distinguishers = ref (SID_checks.M.empty : SID_checks.distinguisher SID_checks.M.t)

let distinguisher name =
  let open SID_checks in
  let pred = find_user_defined name in
  if M.exists (fun (id1, id2) d ->
    if InductiveDefinition.equal pred id1 || InductiveDefinition.equal pred id2 then d = Field
    else false
  ) !distinguishers then Field
  else Sort



(** {2 Operations over dependency graph *)

let dg = ref DependencyGraph.empty

let is_self_recursive name = match find name with
  | Builtin _ -> true (* Conservatively assume true *)
  | UserDefined id -> DependencyGraph.is_self_recursive !dg id

let init () =
  let g = DependencyGraph.compute () in
  Logger.dump DependencyGraph.output "predicate_graph.dot" g;
  dg := g

let normalise () =
  let g = DependencyGraph.normalise !dg in
  Logger.dump DependencyGraph.output "predicate_graph_normalised.dot" g;
  dg := g
  (* TODO: keep or not?
     sid := M.filter (fun name _ -> is_self_recursive name) !sid
  *)

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


(** {2 Preprocessing *)

let preprocess name sl_graph instance = match find name with
  | Builtin (module B : BUILTIN) -> B.preprocess sl_graph instance
  | _ -> None

let preprocess_user_definitions fn =
  sid := M.filter_map (fun _ id -> match id with
    | Builtin _ -> Some id
    | UserDefined id -> match fn id with
      | None -> None
      | Some id -> Some (UserDefined id)
  ) !sid


(** {2 Parsing} *)


let instantiate heap_sort name operands = match find name with
  | Builtin (module B : BUILTIN) -> B.instantiate heap_sort operands
  | UserDefined id -> Result.Ok (SL.mk_predicate name operands)

(** {2 Bounds} *)

let cache = ref PredicateAbstraction.M.empty

let sl_graph name instance = match find name with
  | Builtin (module B : BUILTIN) -> B.sl_graph instance
  | UserDefined id -> SL_graph0.empty

let compute_aux phi g id x a =
  let open InductiveDefinition in
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
  let open PredicateAbstraction in
  if c then (Float.of_int a.stable_size) else a.fixpoint_size

let term_bound phi g heap_sort x =
  let sort = SL.Term.get_sort x in
  fold (fun name pred acc ->
   let bound = match pred with
    | Builtin (module B : BUILTIN) -> B.term_bound phi heap_sort x
    | UserDefined id ->
      let abstraction = PredicateAbstraction.M.find id !cache in
      if Sort.equal (SL.Variable.get_sort abstraction.root) sort then compute_aux phi g id x abstraction
      else Float.one
   in
   max acc bound
) !sid Float.one

let additional_bounds phi =
  M.fold (fun _ pred acc -> match pred with
    | Builtin (module B : BUILTIN) -> acc + B.additional_bound phi
    | UserDefined id -> 0
  ) !sid 0

let abstraction name =  match find name with
  | UserDefined id -> PredicateAbstraction.M.find id !cache


let unfolding_depth phi g name xs = match find name with
  | UserDefined id -> (PredicateAbstraction.M.find id !cache).unfolding_depth
  (*  let abstraction = PredicateAbstraction.find id !cache in
    compute_aux phi g id (List.hd xs) abstraction
  *)

let stable_depth name = match find name with
  | UserDefined id -> (PredicateAbstraction.M.find id !cache).stable_size

let alloc name = match find name with
  | UserDefined id -> (PredicateAbstraction.M.find id !cache).unfolding_depth

let param_conditions name params =
  let abstr = abstraction name in
  let alloc = PredicateAbstraction.get_must_allocated ~params abstr in
  let pairwise =
    List_utils.diagonal_product alloc
    |> List.map (fun (x, y) -> SL.mk_distinct2 x y)
  in
  let nils = List.map (SL.mk_distinct2 SL.Term.nil) alloc in
  pairwise @ nils




(** {2 Translation} *)

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

let get_structs visited get_continue name = match find name with
  | Builtin (module B : BUILTIN) -> B.struct_defs
  | UserDefined id ->
    if BatList.mem_cmp String.compare name visited then []
    else get_continue (name::visited) @@ InductiveDefinition.instantiate_formals id

let id_map () =
  M.fold (fun name pred acc -> match pred with
    | Builtin _ -> acc
    | UserDefined id -> M.add name id acc
  ) !sid M.empty

let unfold name = InductiveDefinition.unfold (id_map ()) (find_user_defined name)
let unfold_guided name = InductiveDefinition.unfold_guided (id_map ()) (find_user_defined name)


(** {2 Model checking} *)

let model_check name instance sh = match find name with
  | Builtin (module B : BUILTIN) -> B.model_check instance sh
  | UserDefined _ -> failwith "TODO: model check UID"

let compute_footprints name instance sh = match find name with
  | Builtin (module B : BUILTIN) -> B.compute_footprints instance sh
  | UserDefined _ -> failwith "TODO: compute_footprints UID"
