open ID
open ID_sig

module L = Logger.Make
  (struct let name = "SID" let level = 1 end)

include SID0

module Logger = L

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

(** {2 Preprocessing *)

let inline name xs = match find name with
  | UserDefined id when not @@ is_self_recursive id.name ->
    (* TODO: move to preprocessor
    let id = InductiveDefinition.map_cases PreciseToImprecise.to_precise id in*)
    Some (InductiveDefinition.instantiate ~refresh:true id xs)
  | _ -> None

let preprocess name sl_graph instance = match find name with
  | Builtin (module B : BUILTIN) -> B.preprocess sl_graph instance
  | _ -> None

let preprocess_user_definitions fn =
  sid := M.map (function id -> match id with
    | Builtin _ -> id
    | UserDefined id -> UserDefined (fn id)
  ) !sid


(** {2 Parsing} *)

let instantiate heap_sort name operands = match find name with
  | Builtin (module B : BUILTIN) -> B.instantiate heap_sort operands
  | UserDefined id -> Result.Ok (SL.mk_predicate name operands)

(** {2 Bounds} *)

module MM = InductiveDefinition.Map

let cache = ref (MM.empty : Float.t MM.t)

let sl_graph name instance = match find name with
  | Builtin (module B : BUILTIN) -> B.sl_graph instance
  | UserDefined _ -> SL_graph0.empty

let term_bound phi heap_sort x = fold (fun name pred acc ->
   let bound = match pred with
    | Builtin (module B : BUILTIN) -> B.term_bound phi heap_sort x
    | UserDefined id -> MM.find id !cache
   in
   max acc bound
) !sid Float.one

let additional_bounds phi =
  M.fold (fun name pred acc -> match pred with
    | Builtin (module B : BUILTIN) -> acc + B.additional_bound phi
    | UserDefined id -> 0
  ) !sid 0


let unfolding_depth name = match find name with
  | UserDefined id -> MM.find id !cache

(** {2 Translation} *)

module Translation (E : Translation_sig.ENCODING) = struct

  let translate name (context : E.Context.t) instance domain sxs = match find name with
    | Builtin (module B : BUILTIN) ->
      let module T = B.Translation(E) in
      let bound =
        B.Bound.compute context.sl_graph context.phi instance context.location_bounds in
      T.translate context instance domain sxs bound
    | UserDefined id ->
      Utils.internal_error
        "User-defined inductive predicate is not unfolded before translation to SMT"

end

(*
let rule_size phi = match SL.view phi with
 | Star (xs) -> List.length @@ List.filter SL.is_pointer xs
*)

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
