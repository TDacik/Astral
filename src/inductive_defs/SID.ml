open ID
open ID_sig

module L = Logger.Make
  (struct let name = "SID" let level = 1 end)

include SID0

module Logger = L

(** {2 Operations over dependency graph *)

let dg = ref DependencyGraph.empty

let is_self_recursive name = match find name with
  | Builtin _ -> failwith "TODO"
  | UserDefined id -> DependencyGraph.is_self_recursive !dg id

let init () =
  let g = DependencyGraph.compute () in
  Logger.dump DependencyGraph.output "predicate_graph.dot" g;
  dg := g

let normalise () =
  let g = DependencyGraph.normalise !dg in
  Logger.dump DependencyGraph.output "predicate_graph_normalised.dot" g;
  dg := g;
  sid := M.filter (fun name _ -> is_self_recursive name) !sid

(** {2 Preprocessing *)

let inline name xs = match find name with
  | UserDefined id when not @@ is_self_recursive id.name ->
    (* TODO: move to preprocessor
    let id = InductivePredicate.map_cases PreciseToImprecise.to_precise id in*)
    Some (InductivePredicate.instantiate ~refresh:true id xs)
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

module MM = InductivePredicate.Map

(*let graph = ref *)
let cache = ref (MM.empty : Int.t MM.t)

let sl_graph name instance = match find name with
  | Builtin (module B : BUILTIN) -> B.sl_graph instance
  | UserDefined _ -> SL_graph0.empty

let term_bound phi heap_sort x = fold (fun name pred acc ->
   let bound = match pred with
    | Builtin (module B : BUILTIN) -> B.term_bound phi heap_sort x
    | UserDefined id -> Float.of_int @@ MM.find id !cache
   in
   max acc bound
) !sid Float.one

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

let rule_size phi = match SL.view phi with
 | Star (xs) -> List.length @@ List.filter SL.is_pointer xs

(** TODO: compute how many locations the unfolding consumes *)
let rec unfold name xs n =
  let id = find_user_defined name in
  if n = 0 then InductivePredicate.unfold_finite id xs
  else SL.map_view (function
    | Predicate (name', ys, _) ->
      Logger.debug "Unfolding %s (remaining %d)\n" name' (n-1);
      unfold name' ys (n-1)
  ) (InductivePredicate.instantiate ~refresh:true id xs)

let rec unfold_synchronised g name xs n =
  let id = find_user_defined name in
  if n = 0 then InductivePredicate.unfold_finite id xs
  else SL.map_view (function
    | Predicate (name', ys, _) ->
      Logger.debug "Unfolding %s (remaining %d)\n" name' (n-1);
      unfold_synchronised g name' ys (n-1)
  ) (InductivePredicate.instantiate ~refresh:true id xs)

(** {2 Model checking} *)

let model_check name instance sh = match find name with
  | Builtin (module B : BUILTIN) -> B.model_check instance sh
  | UserDefined _ -> failwith "TODO: model check UID"

let compute_footprints name instance sh = match find name with
  | Builtin (module B : BUILTIN) -> B.compute_footprints instance sh
  | UserDefined _ -> failwith "TODO: compute_footprints UID"
