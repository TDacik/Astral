(* Generalisation of rules using anti-unification.
 *
 * Example:
 *
 *   tree(x, h) =
 *     | x = h
 *     | x != h * x -> <l, r> * tree(l, h) * tree(r)
 *     | x != h * x -> <l, r> * tree(r, h) * tree(l)
 *
 * will be rewritten to:
 *
 *   tree(x, h) =
 *     | x = h
 *     | x != h * E c1, 2.
 *       ((c1 = r * c2 = l) \/ (c1 = l * c2 = r)) * x -> <l, r> * tree(c1, h) * tree(c2)
 *
 * TODO: generalise for n > 2.
 * TODO: is the implementation sound?
 * TODO: add tests
 * *)

open SL

module Logger = Logger.Make(struct let name = "RuleGeneralisation" let level = 2 end)

module Substition = struct

  type t = SL.Term.MonoList.t SL.Variable.Map.t

  let domain = SL.Variable.Map.keys

  let empty = SL.Variable.Map.empty

  let join = SL.Variable.Map.union (fun _ _ _ -> failwith "Substitions not disjoint")

  let apply_reverse rule1 rule2 subst =
    (* TODO *)
    let get_parts psi = match SL.view psi with
      | Star args -> SL.Set.of_list args
      | _ -> SL.Set.singleton psi
    in
    let args1 = get_parts rule1 in
    let args2 = get_parts rule2 in
    let common = SL.Set.inter args1 args2 in
    let rest1 = SL.Set.diff args1 common in
    let rest2 = SL.Set.diff args2 common in

    (* TODO: does this work with SL terms?? *)
    let modified = SL.Variable.Map.fold (fun var terms acc ->
      let subformula = SL.of_term @@ List.nth terms 0 in
      SL.replace_subformula acc ~subformula ~by:(SL.of_var var)
    ) subst (SL.mk_star @@ SL.Set.elements rest1)
    in
    SL.mk_star (modified :: SL.Set.elements common)

  let to_formula subst =
    if SL.Variable.Map.is_empty subst then SL.tt
    else
      let [x, [a1; a2]; y, [b1; b2]] = SL.Variable.Map.bindings subst in
      SL.mk_or [
        SL.mk_star [SL.mk_eq2 (SL.Term.of_var x) a1; SL.mk_eq2 (SL.Term.of_var y) b1];
        SL.mk_star [SL.mk_eq2 (SL.Term.of_var x) a2; SL.mk_eq2 (SL.Term.of_var y) b2];
      ]

end

(** Antiunify two terms of the same sort. *)
let anti_unify_term t1 t2 =
  Logger.debug "Terms:\n  %s\n  %s\n" (SL.Term.show t1) (SL.Term.show t2);
  if SL.Term.equal t1 t2 then SL.Variable.Map.empty
  else
    let var = SL.Variable.mk_fresh "u" @@ SL.Term.get_sort t1 in
    SL.Variable.Map.singleton var [t1; t2]

let anti_unify_terms terms1 terms2 =
  Logger.debug "Term lists:\n  %s\n  %s\n" (SL.Term.show_list terms1) (SL.Term.show_list terms2);
  let sorts1 = List.map SL.Term.get_sort terms1 in
  let sorts2 = List.map SL.Term.get_sort terms2 in
  if not @@ List.equal Sort.equal_mod_nil sorts1 sorts2 then
    let _ =Logger.debug "Length differs\n" in
    None
  else
    Option.some
    @@ List.fold_left2 (fun acc t1 t2 ->
      Substition.join acc @@ anti_unify_term t1 t2
    ) SL.Variable.Map.empty terms1 terms2

let rec anti_unify rule1 rule2 =
  Logger.debug "Formulae :\n  %s\n  %s\n" (SL.show rule1) (SL.show rule2);
  match SL.view rule1, SL.view rule2 with
  | PointsTo (x1, s1, ys1), PointsTo (x2, s2, ys2) when MemoryModel.StructDef.equal s1 s2 ->
    anti_unify_terms (x1 :: ys1) (x2 :: ys2)
  | Predicate (pred1, xs1, []), Predicate (pred2, xs2, []) when String.equal pred1 pred2 ->
    anti_unify_terms xs1 xs2
  | Eq xs1, Eq xs2 -> anti_unify_terms xs1 xs2
  | Distinct xs1, Distinct xs2 -> anti_unify_terms xs1 xs2
  | Star psis1, Star psis2 when List.compare_lengths psis1 psis2 = 0 ->
    let psis1 = SL.MonoList.sort psis1 in
    let psis2 = SL.MonoList.sort psis2 in
    List.fold_left2 (fun acc psi1 psi2 ->
      match anti_unify psi1 psi2, acc with
        | Some s, Some acc -> Option.some @@ Substition.join acc s
        | None, Some _ ->
          Logger.debug "Failed for %s and %s\n" (SL.show psi1) (SL.show psi2);
          None
        | _, _ -> None
    ) (Some SL.Variable.Map.empty) psis1 psis2
  | _ -> None

(* TODO:
   - no change
   - n > 2
   - antiunified part *)

let apply_pair rule1 rule2 =
  match anti_unify rule1 rule2 with
    | None ->
      Logger.debug "Failed for rules:\n  %s\n  %s"
        (SL.show rule1)
        (SL.show rule2);
      [rule1; rule2]
    | Some res ->
      let body = Substition.apply_reverse rule1 rule2 res in
      let choice = Substition.to_formula res in
      let domain = Substition.domain res in
      [SL.mk_exists domain @@ SL.mk_star [body; choice]]


let apply id =
  let open InductiveDefinition in
  Logger.debug "starting\n";
  let rules = id.inductive_cases in
  let rules'= match rules with
    | [r1; r2] -> apply_pair r1 r2
    | rs -> rs
  in
  InductiveDefinition.mk id.name id.header (SL.mk_or @@ id.base_cases @ rules')
