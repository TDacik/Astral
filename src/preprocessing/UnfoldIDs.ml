(* Pass for full unfolding of inductive predicates.
 *
 * TODO: implement refined bounds
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

module Logger = Logger.Make(struct let name = "unfolder" let level = 1 end)

let unfold_sat sid phi name xs =
  let bound = GlobalSID.stable_depth name in
  Logger.debug "Unfolding predicate %s(%s) up to depth %d\n"
    name (SL.Term.show_list xs) (bound);
  SID.unfold sid name xs bound

let unfold_predicate_lhs sid phi lhs name xs =
  let bound = GlobalSID.unfolding_depth name in
  Logger.debug "Unfolding predicate %s(%s): %d\n" name (SL.Term.show_list xs) bound;
  SID.unfold sid name xs bound

let unfold_lhs sid g heap_sort phi lhs rhs = SL.map_view (function
  | Predicate (name, xs, _) when SID.is_user_defined sid name ->
    `Modify (unfold_predicate_lhs sid phi lhs name xs)
  | _ -> `Skip
) lhs

let unfold_sat sid lhs = SL.map_view (function
  | Predicate (name, xs, _) when SID.is_user_defined sid name ->
    `Modify (unfold_sat sid lhs name xs)
  | _ -> `Skip
) lhs

let unfold_rhs sid ctx sl_graph lhs rhs =
  let open Backend_sig in
  let open Translation_sig in
  let module Backend = (val ConfigReader.get_backend () : BACKEND) in
  let module IncrementalBackend = (val ConfigReader.get_incremental_backend () : BACKEND) in
  let module Encoding = (val ConfigReader.get_encoding () : ENCODING) in
  let module Translation = Translation.Make(Encoding)(Backend) in
  let module Unfolder = IncrementalUnfolding.Make(Encoding)(IncrementalBackend) in

  let lhs_t = Translation.translate {ctx with phi = lhs} in (* TODO: check*)
  Unfolder.unfold ctx lhs lhs_t rhs

let unfold_default ctx phi =
  let open Context in
  let sid = GlobalSID.get () in
  match SL.view phi with
    | _ when SL.is_symbolic_heap phi ->
      {ctx with phi = unfold_sat sid phi}
    | GuardedNeg (lhs, rhs) ->
      (* Here we assume that quantifier elimination for LHS was already performed. *)
      let sl_graph = SL_graph.compute lhs in
      let ctx_lhs = {ctx with phi = Simplifier.simplify @@ unfold_lhs sid sl_graph ctx.heap_sort phi lhs rhs} in
      let lhs = ctx_lhs.phi in
      (* TODO: avoid duplicated computation *)
        let open Context in
        let bounds = LocationBounds.compute lhs ctx.raw_input.heap_sort sl_graph in
        let ctx = {ctx with location_bounds = bounds} in
      let rhs = unfold_rhs sid ctx sl_graph lhs rhs in
      {ctx with phi = SL.mk_gneg lhs rhs; model_adapter = ctx_lhs.model_adapter}
    | False | True -> ctx
    | _ -> assert false (* Should be catched earlier *)

let apply ctx phi =
  if List.is_empty @@ GlobalSID.get_user_defined () then ctx
  else unfold_default ctx phi

let apply_ctx ctx = apply ctx ctx.phi
