(* Simple simplification by applying smart constructors.
 *
 * TODO: add predicate simplication rules.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

module VarSet = SL.Variable.Set

let simplify ?(dont_care=[]) phi = SL.map_view (function
  | SL.And psis -> `Modify (SL.mk_and psis)
  | SL.Or psis -> `Modify (SL.mk_or psis)
  | SL.Star psis -> `Modify (SL.mk_star psis)
  | SL.Eq xs -> `Modify (SL.mk_eq xs)
  | SL.Distinct xs -> `Modify (SL.mk_distinct xs)
  | SL.Not (psi) -> `Modify (SL.mk_not psi)
  | SL.GuardedNeg (psi1, psi2) -> `Modify (SL.mk_gneg psi1 psi2)
  | SL.Ite (c, t, e) -> `Modify (SL.mk_ite c t e)
  | SL.Exists (xs, psi) -> `Modify (SL.mk_exists xs psi)
  | _ -> `Skip
) phi

let check_suitable_sorts xs =
  let sort = List.map SL.Term.get_sort xs in
  List.for_all Sort.is_infinite sort

let classify_relevant_vars phi =
  let all_vars = VarSet.of_list @@ SL.free_vars ~with_nil:false phi in
  let phi' =
    SL.map_view (function
      | SL.Distinct xs when check_suitable_sorts xs -> `Modify SL.emp
      | _ -> `Skip) phi
  in
  let relevant_vars = VarSet.of_list @@ SL.free_vars ~with_nil:false phi' in
  let irrelevant_vars = VarSet.diff all_vars relevant_vars in
  (relevant_vars, irrelevant_vars)

let keep_term relevant term = match SL.Term.view term with
  | Var v -> VarSet.mem v relevant
  | _ -> true

let eliminate_distinct adapter phi =
  let keep, discard = classify_relevant_vars phi in
  let adapter = ModelAdapter.add_fresh_vars adapter discard in
  adapter, SL.map_view (function
    | SL.Distinct xs -> `Modify (SL.mk_distinct @@ List.filter (keep_term keep) xs)
    | _ -> `Skip
  ) phi

let rec only_in_disequalities phi x =
  let f phi = only_in_disequalities phi x in
  let x = SL.Term.of_var x in
  match SL.view phi with
  | SL.Star psis -> List.for_all f psis
  | SL.Eq xs -> not @@ SL.Term.MonoList.mem x xs
  | SL.PointsTo (x, _, ys) -> not @@ SL.Term.MonoList.mem x (x :: ys)
  | SL.Predicate (_, xs, _, _) -> not @@ SL.Term.MonoList.mem x xs
  | SL.Distinct _ | Emp -> true
  | _ -> assert false

let remove_disequalities phi =
  let candidate_vars =
    SL.free_vars ~with_nil:false phi
    |> List.filter (only_in_disequalities phi)
    |> List.map SL.Term.of_var
  in
  SL.map_view (function
    | SL.Distinct [x; y] when SL.Term.MonoList.are_disjoint candidate_vars [x; y] ->
      `Modify SL.emp
    | _ -> `Skip
  ) phi

let simplify_ctx ctx =
  let open Context in
  let phi = simplify ctx.phi in
  let model_adapter, phi = ctx.model_adapter, phi (* eliminate_distinct ctx.model_adapter phi*) in
  {ctx with phi; model_adapter}
