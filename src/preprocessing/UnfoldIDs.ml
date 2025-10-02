(* Pass for full unfolding of inductive predicates.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

module Logger = Logger.Make(struct let name = "unfolder" let level = 1 end)

let must_allocate_lhs phi heap_sort lhs g name =
  let _, atoms = SL.as_symbolic_heap lhs in
  List.map (fun atom -> match SL.view atom with
    | PointsTo _ -> 1.0
    | Predicate (name, xs, _) -> GlobalSID.alloc name phi g heap_sort xs
    | _ -> 0.0
  ) atoms
  |> List.map Float.floor
  |> List.map Float.to_int
  |> BatList.sum

(*
let unfolding_depth lhs g pred_name ys =
    let _, atoms = SL.as_symbolic_heap lhs in
    BatList.sum @@ BatList.map (fun atom -> match SL.view atom with
      | PointsTo _ -> 1
      | Predicate (name, _, _) -> SID.unfolding
        begin match SID.distinguisher name with
          | Field ->
            if List.for_all (SL_graph.must_neq g SL.Term.nil) ys then SID.stable_depth name
            else SID.unfolding_depth name
          | _ -> SID.unfolding_depth name
        end
      | _ -> 0
    ) atoms
*)

(** Bound on unfolding of lhs when rhs is atomic. *)
let max_unfold_bound_lhs rhs default = default
(*
(* TODO: remove already in bounds *)
let max_unfold_bound_lhs rhs default =
  match SL.pointer_size rhs with
    | None -> default
    | Some n -> n + 1 *)

(** Bound on unfolding of rhs when lhs is atomic. *)
let max_unfold_bound_rhs lhs default = default
 (*
  match SL.pointer_size lhs with
    | None -> default
    | Some n -> n
 *)

let unfold_predicate_lhs sid phi lhs max_bound name xs =
  Logger.debug "Unfolding predicate %s(%s) up to depth %d\n"
    name (SL.Term.show_list xs) (max_bound);
  SID.unfold sid name xs (max_bound)

let unfold_sat sid phi name xs =
  let abstraction = GlobalSID.abstraction name in
  let bound = PredicateAbstraction.(abstraction.unfolding_depth) in
  Logger.debug "Unfolding predicate %s(%s) up to depth %d\n"
    name (SL.Term.show_list xs) (bound);
  SID.unfold sid name xs bound

let unfold_lhs sid g heap_sort bound phi lhs rhs = SL.map_view (function
  | Predicate (name, xs, _) when SID.is_user_defined sid name ->
    let self = GlobalSID.unfolding_depth phi g name xs in
    let alloc = must_allocate_lhs phi heap_sort lhs g name in
    let default = (LocationBounds.sum_of_allocated bound) - alloc + self in
    let bound = max_unfold_bound_lhs rhs default in
    `Modify (unfold_predicate_lhs sid phi lhs bound name xs)
  | _ -> `Skip
) lhs

let unfold_sat sid lhs = SL.map_view (function
  | Predicate (name, xs, _) when SID.is_user_defined sid name ->
    `Modify (unfold_sat sid lhs name xs)
  | _ -> `Skip
) lhs

let unfold_rhs sid ctx lhs rhs =
  let open Backend_sig in
  let open Translation_sig in
  let module Backend = (val Options.backend () : BACKEND) in
  let module IncrementalBackend = (val Options.incremental_backend () : BACKEND) in
  let module Encoding = (val Options.encoding () : ENCODING) in
  let module Translation = Translation.Make(Encoding)(Backend) in
  let module Unfolder = IncrementalUnfolding.Make(Encoding)(IncrementalBackend) in
  let lhs_t = Translation.translate {ctx with phi = lhs} in (* TODO: check*)
  Debug.translated ~suffix:"LHS" lhs_t;
  Unfolder.unfold ctx lhs_t rhs

let apply_aux ctx phi =
  let sid = GlobalSID.get () in
  let open Context in
  let location_bound = ctx.location_bounds in
  Logger.debug "Unfolding %s\n" (SL.show phi);
  match SL.view phi with
    | _ when SL.is_symbolic_heap phi ->
      {ctx with phi = unfold_sat sid phi}
    | GuardedNeg (lhs, rhs) ->
      (* Here we assume that quantifier elimination for LHS was already performed. *)
      let sl_graph = SL_graph.compute lhs in
      let ctx_lhs = {ctx with phi = Simplifier.simplify @@ unfold_lhs sid sl_graph ctx.heap_sort location_bound phi lhs rhs} in
      let lhs = ctx_lhs.phi in
      let rhs = unfold_rhs sid ctx lhs rhs in
      {ctx with phi = SL.mk_gneg lhs rhs; model_adapter = ctx_lhs.model_adapter}
    | False | True -> ctx
    | _ -> assert false (* Should be catched earlier *)

let apply ctx phi =
  if List.is_empty @@ GlobalSID.get_user_defined () then ctx
  else apply_aux ctx phi

let apply_ctx ctx = apply ctx ctx.phi
