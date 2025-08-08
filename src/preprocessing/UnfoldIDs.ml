module Logger = Logger.Make(struct let name = "unfolder" let level = 1 end)

let must_allocate_lhs phi lhs g name =
  let _, atoms = SL.as_symbolic_heap lhs in
  List.map (fun atom -> match SL.view atom with
    | PointsTo _ -> 1
    | Predicate (name, xs, _) -> SID.alloc name
    | _ -> 0
  ) atoms
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

let unfold_predicate_lhs phi lhs loc_bound name xs =
  let g = SL_graph.compute lhs in
  let self = SID.unfolding_depth phi g name xs in
  let alloc = must_allocate_lhs phi lhs g name in
  let bound = (LocationBounds.sum loc_bound) - alloc + self - 1 in (* -1 for nil *)
  (*let bound = Float.to_int @@ SID.unfolding_depth name in*)
    Logger.debug "Unfolding predicate %s(%s) up to depth %d\n"
    name (SL.Term.show_list xs) (bound);
  SID.unfold name xs (bound)

let unfold_sat phi name xs =
  let abstraction = SID.abstraction name in
  let bound = PredicateAbstraction.(abstraction.unfolding_depth) in
  Logger.debug "Unfolding predicate %s(%s) up to depth %d\n"
    name (SL.Term.show_list xs) (bound);
  SID.unfold name xs bound

let unfold_predicate_rhs phi loc_bound g name xs =
  let def = SID.get_definition name in
  let max_bound = LocationBounds.sum loc_bound - 1 in
  (*let max_bound = List.length @@ SL.free_vars phi in*)
  (*let max_bound = unfolding_depth phi in*)
  Logger.debug "Unfolding predicate %s(%s) up to depth %d\n"
    name (SL.Term.show_list xs) max_bound;
  SID.unfold_guided name g xs max_bound

let unfold_lhs bound phi lhs = SL.map_view (function
  | Predicate (name, xs, _) when not @@ SID.is_builtin name ->
    `Modify (unfold_predicate_lhs phi lhs bound name xs)
  | _ -> `Skip
) lhs

let unfold_sat lhs = SL.map_view (function
  | Predicate (name, xs, _) when not @@ SID.is_builtin name ->
    `Modify (unfold_sat lhs name xs)
  | _ -> `Skip
) lhs

let unfold_rhs ctx bound lhs rhs =
  let open Backend_sig in
  let open Translation_sig in
  (* Other backends do not support incremental solving *)
  let module Backend = (val Options.backend () : BACKEND) in
  let module IncrementalBackend = (val Options.incremental_backend () : BACKEND) in
  let module Encoding = (val Options.encoding () : ENCODING) in
  let module Translation = Translation.Make(Encoding)(Backend) in
  let module Unfolder = IncrementalUnfolding.Make(Encoding)(IncrementalBackend) in
  let lhs = Translation.translate {ctx with phi = lhs} in (* TODO: check*)
  Debug.translated ~suffix:"LHS" lhs;
  Unfolder.unfold bound lhs rhs

let apply_aux ctx phi =
  let open Context in
  let location_bound = ctx.location_bounds in
  Logger.debug "Unfolding %s\n" (SL.show phi);
  match SL.view phi with
    | _ when SL.is_symbolic_heap phi ->
      {ctx with phi = unfold_sat phi}
    | GuardedNeg (lhs, rhs) ->
      let ctx_lhs = QuantifierElimination.apply_ctx @@
        {ctx with phi = Simplifier.simplify @@ unfold_lhs location_bound phi lhs}
      in
      let lhs = ctx_lhs.phi in
      let sl_graph = SL_graph.compute lhs in
      let rhs = unfold_rhs ctx ctx lhs rhs in
      {ctx with phi = SL.mk_gneg lhs rhs; model_adapter = ctx_lhs.model_adapter}
    | _ -> assert false (* Should be catched earlier *)

let apply ctx phi =
  if not @@ SLID.has_user_defined_predicates phi then ctx
  else apply_aux ctx phi

let apply_ctx ctx = apply ctx ctx.phi
