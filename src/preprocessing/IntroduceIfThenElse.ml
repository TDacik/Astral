(* Introduction of the if-then-else operator instead of disjunctions.
 *
 * TODO: consider n-ary disjunctions?
 * TODO: simplify
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

module Logger = Logger.Make (struct
  let name = "ITE intro"
  let level = 2
end)

let saturate conds =
  let module R = List_utils.Relation(SL.Term) in
  let eqs = List.filter_map SL.as_equality conds in
  let res = conds @ List.map (fun (x, y) -> SL.mk_eq2 x y) @@ R.transitive_closure_list eqs in
  res

let rec candidate_conditions phi = match SL.view phi with
  | Emp -> []
  | Eq _ | Distinct _ -> [phi]
  | PointsTo (x, def, ys) ->
    let fields = MemoryModel.StructDef.get_fields def in
    List.map2 (fun f y ->
        let rhs = SL.Term.mk_heap_term f x in
        SL.mk_eq2 y rhs
      ) fields ys
    |> (fun tl -> SL.mk_distinct [x; SL.Term.nil] :: tl)
  | Star psis | And psis -> List.concat_map candidate_conditions psis
  | Exists (xs, psi) ->
    List.filter (SL.is_ground' ~forbidden:xs) @@ candidate_conditions psi
  | Predicate (pred, xs, []) ->
    let unfolding = GlobalSID.unfold pred xs 1 in
    candidate_conditions unfolding
  | Or psis ->
    let cs = List.map candidate_conditions psis in
    SL.MonoList.inter_list cs
  | Ite (_, t, e) ->
    let c1 = candidate_conditions t in
    let c2 = candidate_conditions e in
    SL.MonoList.inter c1 c2
  | _ -> failwith @@ SL.show phi

let candidate_conditions phi =
  candidate_conditions phi
  |> saturate
  |> List.filter (fun psi -> match SL.view psi with True -> false | _ -> true)

let is_contradiction atom1 atom2 = match SL.view atom1, SL.view atom2 with
  | Eq xs, Distinct ys
  | Distinct xs, Eq ys -> SL.Term.Set.(equal (of_list xs) (of_list ys))
  | _ -> false

let rec split phi = match SL.view phi with
  | Eq _ | Distinct _ | PointsTo _ | Predicate _ -> [], [phi]
  | Star psis | And psis -> [], psis
  | Ite _ -> [], [phi] (* TODO? *)
  | Exists (xs, body) ->
    let xs', args = split body in
    xs @ xs', args

let find_ite_condition forbidden lhs rhs =
  let conds1 = List.filter (SL.is_ground' ~forbidden) @@ candidate_conditions lhs in
  let conds2 = List.filter (SL.is_ground' ~forbidden) @@ candidate_conditions rhs in
  try
    let c1, c2 =
      BatList.cartesian_product conds1 conds2
      |> List.find (fun (a1, a2) -> is_contradiction a1 a2)
    in
    let qs1, args1 = split lhs in
    let qs2, args2 = split rhs in
    let lhs' = SL.mk_exists qs1 @@ SL.mk_star @@ BatList.remove_if (SL.equal c1) args1 in
    let rhs' = SL.mk_exists qs2 @@ SL.mk_star @@ BatList.remove_if (SL.equal c2) args2 in
    Some (c1, lhs', rhs')
  with Not_found -> None

let find_ite_condition_n forbidden one many =
  let conds_1 = List.filter (SL.is_ground' ~forbidden) @@ candidate_conditions one in
  let conds_n = List.map (fun x -> List.filter (SL.is_ground' ~forbidden) @@ candidate_conditions x) many in
  List.find (fun candidate ->
    List.for_all (fun x -> List.exists (fun y -> is_contradiction candidate y) x) conds_n
  ) conds_1

let apply_bin forbidden_vars lhs rhs =
  let res = find_ite_condition forbidden_vars lhs rhs in
  match res with
    | Some (c, lhs', rhs') ->
      `Modify (SL.mk_ite c lhs' rhs')
    | None -> `Skip

let rec split_based_on_equality psis conds = match conds, psis with
  | _, [psi] -> psi
  | [], _ -> failwith "..."
  | c :: conds_rest, _ ->
    (* TODO: could be done more efficiently *)
    let tt, ff = List.partition (fun (psi : SL.t) -> List.mem c @@ candidate_conditions psi) psis in
    match tt, ff with
    | [], [] -> assert false
    | [], xs -> SL.mk_or xs
    | xs, [] -> SL.mk_or xs
    | _, _ ->
      SL.mk_ite c
        (split_based_on_equality tt conds_rest)
        (split_based_on_equality ff conds_rest)

let apply_general forbidden psis =
  let separators = List_utils.diagonal_product psis in
  let separators =
    List.map (fun (case1, case2) -> match find_ite_condition forbidden case1 case2 with
      | Some (c, _, _) -> Some c
      | None -> None
    ) separators
  in
  if List.for_all Option.is_some separators then
    let separators = List.map Option.get separators in
    `Modify (split_based_on_equality psis separators)
  else `Skip

let apply ?(forbidden_vars=[]) phi =
  SL.map_view (function
    | Or [lhs; rhs] -> apply_bin forbidden_vars lhs rhs
    | Or psis -> apply_general forbidden_vars psis
    | _ -> `Skip
    ) phi

let apply_ctx ?(forbidden_vars=[]) ctx =
  Context.{ctx with phi = apply ~forbidden_vars ctx.phi}
