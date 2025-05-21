(* Introduction of the if-then-else operator instead of disjunctions.
 *
 * TODO: consider n-ary disjunctions?
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

module Logger = Logger.Make (struct
  let name = "ITE intro"
  let level = 2
end)

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
  | Star psis -> List.concat_map candidate_conditions psis
  | Exists (xs, psi) ->
    List.filter (SL.is_ground' ~forbidden:xs) @@ candidate_conditions psi
  | Predicate (pred, xs, []) -> SID.param_conditions pred xs
  | _ -> failwith @@ SL.show phi

let is_contradiction atom1 atom2 = match SL.view atom1, SL.view atom2 with
  | Eq xs, Distinct ys
  | Distinct xs, Eq ys -> SL.Term.Set.(equal (of_list xs) (of_list ys))
  | _ -> false

let find_ite_condition forbidden lhs rhs =
  let conds1 = List.filter (SL.is_ground' ~forbidden) @@ candidate_conditions lhs in
  let conds2 = List.filter (SL.is_ground' ~forbidden) @@ candidate_conditions rhs in
  try
    let c1, c2 =
      BatList.cartesian_product conds1 conds2
      |> List.find (fun (a1, a2) -> is_contradiction a1 a2)
    in
    let qs1, atoms1 = SL.as_quantified_symbolic_heap lhs in
    let qs2, atoms2 = SL.as_quantified_symbolic_heap rhs in
    let lhs' = SL.mk_exists qs1 @@ SL.mk_star @@ BatList.remove_if (SL.equal c1) atoms1 in
    let rhs' = SL.mk_exists qs2 @@ SL.mk_star @@ BatList.remove_if (SL.equal c2) atoms2 in
    Some (c1, lhs', rhs')
  with Not_found -> None

let apply ?(forbidden_vars=[]) phi =
  SL.map_view (function
    | Or [lhs; rhs] when List.for_all SL.is_symbolic_heap [lhs; rhs] ->
      let res = find_ite_condition forbidden_vars lhs rhs in
      begin match res with
        | Some (c, lhs', rhs') ->
          `Modify (SL.mk_ite c lhs' rhs')
        | None -> `Skip
      end
    | _ -> `Skip
  ) phi

let apply_ctx ?(forbidden_vars=[]) ctx =
  Context.{ctx with phi = apply ~forbidden_vars ctx.phi}
