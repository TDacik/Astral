module Logger = Logger.Make(struct let name = "unfolder" let level = 1 end)

let must_allocate_lhs lhs name =
  let _, atoms = SL.as_symbolic_heap lhs in
  List.map (fun atom -> match SL.view atom with
    | PointsTo _ -> 1
    | Predicate (name, _, _) -> Float.to_int @@ SID.unfolding_depth name
    | _ -> 0
  ) atoms
  |> BatList.sum

let unfold_predicate_lhs lhs loc_bound name xs =
  let self = Float.to_int @@ SID.unfolding_depth name in
  let alloc = must_allocate_lhs lhs name in
  let bound = (LocationBounds.sum loc_bound) - alloc + self - 1 in (* -1 for nil *)
  (*let bound = Float.to_int @@ SID.unfolding_depth name in*)
    Logger.debug "Unfolding predicate %s(%s) up to depth %d\n"
    name (SL.Term.show_list xs) (bound);
  SID.unfold name xs (bound)

let unfolding_depth phi = match SL.view phi with
  | GuardedNeg (lhs, _) ->
    let _, atoms = SL.as_symbolic_heap lhs in
    BatList.kahan_sum @@ BatList.map (fun atom -> match SL.view atom with
      | PointsTo _ -> 1.0
      | Predicate (name, _, _) -> (SID.unfolding_depth name)
      | _ -> 0.0
    ) atoms
  |> Float.to_int

let unfold_predicate_rhs phi loc_bound g name xs =
  let def = SID.get_definition name in
  (*let max_bound = LocationBounds.sum loc_bound in
  let max_bound = List.length @@ SL.free_vars phi in*)
  let max_bound = unfolding_depth phi in
  Logger.debug "Unfolding predicate %s(%s) up to depth %d\n"
    name (SL.Term.show_list xs) max_bound;
  SID.unfold_guided name g xs max_bound

let unfold_lhs bound lhs = SL.map_view (function
  | Predicate (name, xs, _) when not @@ SID.is_builtin name ->
    unfold_predicate_lhs lhs bound name xs
) lhs

let unfold_rhs ctx bound lhs rhs =
  let open Backend_sig in
  let open Translation_sig in
  (* Other backends do not support incremental solving *)
  let module Backend = (val Options.backend () : BACKEND) in
  let module Encoding = (val Options.encoding () : ENCODING) in
  let module Translation = Translation.Make(Encoding)(Backend) in
  let module Unfolder = IncrementalUnfolding.Make(Encoding)(Z3_backend.Init()) in
  let lhs = Translation.translate {ctx with phi = lhs} in (* TODO: check*)
  Unfolder.unfold bound lhs rhs


let apply ctx phi =
  let open Context in
  let location_bound = ctx.location_bounds in
  Logger.debug "Unfolding %s\n" (SL.show phi);
  match SL.view phi with
  | _ when SL.is_symbolic_heap phi -> unfold_lhs location_bound phi
  | GuardedNeg (lhs, rhs) ->
    let lhs = unfold_lhs location_bound lhs in
    let sl_graph = SL_graph.compute lhs in
    let rhs = unfold_rhs ctx ctx lhs rhs in
    SL.mk_gneg lhs rhs
  | _ -> phi

let apply_ctx ctx =
  let open Context in
  {ctx with phi = apply ctx ctx.phi}
