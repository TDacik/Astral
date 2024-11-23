
module Logger = Logger.Make(struct let name = "unfolder" let level = 1 end)

let unfold_predicate_lhs name xs =
  let def = SID.get_definition name in
  let bound = List.length xs in
  Logger.debug "Unfolding predicate %s(%s) up to depth %d\n"
    name (SL.Term.show_list xs) bound;
  InductivePredicate.unfold def xs bound

let unfold_predicate_rhs loc_bound g name xs =
  let def = SID.get_definition name in
  let max_bound = LocationBounds.sum loc_bound in
  Logger.debug "Unfolding predicate %s(%s) up to depth %d\n"
    name (SL.Term.show_list xs) max_bound;
  InductivePredicate.unfold_synchronised g def xs max_bound

let unfold_lhs = SL.map_view (function
  | Predicate (name, xs, _) when not @@ SID.is_builtin name ->
    unfold_predicate_lhs name xs
)

let unfold_rhs bound g = SL.map_view (function
  | Predicate (name, xs, _) when not @@ SID.is_builtin name ->
    unfold_predicate_rhs bound g name xs
)

let apply location_bound phi = match SL.view phi with
  | _ when SL.is_symbolic_heap phi -> unfold_lhs phi
  | GuardedNeg (lhs, rhs) ->
    let lhs = unfold_lhs lhs in
    let sl_graph = SL_graph.compute lhs in
    let rhs = unfold_rhs location_bound sl_graph rhs in
    SL.mk_gneg lhs rhs
  | _ -> phi
