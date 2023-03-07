(* Special preprocessing for formulae originating from Broom bi-abductive analyser.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

(** Sort quantified variables according to size of their domains. *)
let sort_vars vars =
  let domain_size var = match var with
    | SSL.Pure _ -> 2
    | SSL.Var _ -> 3 (* TODO *)
  in
  let compare x1 x2 = Int.compare (domain_size x1) (domain_size x2) in
  List.sort compare vars

(** Replace quantification over boolean variables by boolean connectives. *)
let unfold_bool kind x psi = match x with
  | SSL.Pure (SMT.Variable x) ->
    let x = SMT.Variable x in
    let psi_true = SSL.substitute_pure psi x (SMT.Boolean.mk_true ()) in
    let psi_false = SSL.substitute_pure psi x (SMT.Boolean.mk_false ()) in
    begin match kind with
      | `Forall -> SSL.mk_and [psi_true; psi_false]
      | `Exists -> SSL.mk_or [psi_true; psi_false]
    end
  | _ ->
    begin match kind with
      | `Forall -> SSL.mk_forall [x] psi
      | `Exists -> SSL.mk_exists [x] psi
    end

let is_pure_true phi = SSL.equal phi (SSL.mk_pure @@ SMT.Boolean.mk_true ())
let is_pure_false phi = SSL.equal phi (SSL.mk_pure @@ SMT.Boolean.mk_false ())

let rec remove_iffs phi =
  if SSL.is_iff phi then begin
    let lhs, rhs = SSL.get_iff_operands phi in
    if SSL.is_true lhs then rhs
    else if SSL.is_true rhs then lhs
    else if SSL.is_false lhs then SSL.mk_not rhs
    else if SSL.is_false rhs then SSL.mk_not lhs
    (* TODO: check *)
    else if is_pure_true lhs then rhs
    else if is_pure_true rhs then lhs
    else if is_pure_false lhs then SSL.mk_not rhs
    else if is_pure_false rhs then SSL.mk_not lhs
    else SSL.mk_iff [remove_iffs lhs; remove_iffs rhs]
  end
  else match phi with
  | SSL.Var v -> SSL.Var v
  | SSL.Pure term -> SSL.mk_pure term
  | SSL.Eq xs -> SSL.mk_eq_list xs
  | SSL.Distinct xs -> SSL.mk_distinct_list xs
  | SSL.PointsTo (x, ys) -> SSL.mk_pto_seq x ys
  | SSL.LS (x, y) -> SSL.mk_ls x y
  | SSL.And (psi1, psi2) -> SSL.mk_and [remove_iffs psi1; remove_iffs psi2]
  | SSL.Or (psi1, psi2) -> SSL.mk_or [remove_iffs psi1; remove_iffs psi2]
  | SSL.Not psi -> SSL.mk_not (remove_iffs psi)
  | SSL.GuardedNeg (psi1, psi2) -> SSL.mk_gneg (remove_iffs psi1) (remove_iffs psi2)
  | SSL.Star psis -> SSL.mk_star (List.map remove_iffs psis)
  | SSL.Septraction (psi1, psi2) ->
      SSL.mk_septraction (remove_iffs psi1) (remove_iffs psi2)
  | SSL.Forall (xs, psi) -> SSL.mk_forall xs (remove_iffs psi)
  | SSL.Exists (xs, psi) -> SSL.mk_exists xs (remove_iffs psi)

let unpack (SSL.Pure x) = x

let rec collect_bool_atoms pred phi =
  let collect_bool_atoms = collect_bool_atoms pred in
  if SSL.is_iff phi then begin
    let lhs, rhs = SSL.get_iff_operands phi in
    if pred lhs then [unpack rhs]
    else if pred rhs then [unpack lhs]
    else []
  end
  else match phi with
  | SSL.Var _ | SSL.Pure _ | SSL.Eq _ | SSL.Distinct _ -> []
  | SSL.PointsTo _ | SSL.LS _ | SSL.DLS _ -> []
  | SSL.And (psi1, psi2) -> collect_bool_atoms psi1 @ collect_bool_atoms psi2
  | SSL.Or (psi1, psi2) -> collect_bool_atoms psi1 @ collect_bool_atoms psi2
  | SSL.Not psi -> collect_bool_atoms psi
  | SSL.GuardedNeg (psi1, psi2) -> collect_bool_atoms psi1 @ collect_bool_atoms psi2
  | SSL.Star psis -> List.concat @@ List.map collect_bool_atoms psis
  | SSL.Septraction (psi1, psi2) -> collect_bool_atoms psi1 @ collect_bool_atoms psi2
  | SSL.Forall (xs, psi) -> collect_bool_atoms psi
  | SSL.Exists (xs, psi) -> collect_bool_atoms psi

let replace_iffs phi =
  let trues = collect_bool_atoms SSL.is_true phi in
  let falses = collect_bool_atoms SSL.is_false phi in
  let phi =
    List.fold_left (fun phi x -> SSL.substitute_pure phi x (SMT.Boolean.mk_true ())) phi trues
  in
  List.fold_left (fun phi x -> SSL.substitute_pure phi x (SMT.Boolean.mk_false ())) phi falses

let rec sort_quantifiers = function
  (* TODO: *)
  | SSL.Not (SSL.Eq xs) -> SSL.Distinct xs
  | SSL.Not (SSL.Distinct xs) -> SSL.Eq xs
  | SSL.Var v -> SSL.Var v
  | SSL.Pure term -> SSL.mk_pure term
  | SSL.Eq xs -> SSL.mk_eq_list xs
  | SSL.Distinct xs -> SSL.mk_distinct_list xs
  | SSL.PointsTo (x, ys) -> SSL.mk_pto_seq x ys
  | SSL.LS (x, y) -> SSL.mk_ls x y
  | SSL.And (psi1, psi2) -> SSL.mk_and [sort_quantifiers psi1; sort_quantifiers psi2]
  | SSL.Or (psi1, psi2) -> SSL.mk_or [sort_quantifiers psi1; sort_quantifiers psi2]
  | SSL.Not psi -> SSL.mk_not (sort_quantifiers psi)
  | SSL.GuardedNeg (psi1, psi2) -> SSL.mk_gneg (sort_quantifiers psi1) (sort_quantifiers psi2)
  | SSL.Star psis -> SSL.mk_star (List.map sort_quantifiers psis)
  | SSL.Septraction (psi1, psi2) ->
      SSL.mk_septraction (sort_quantifiers psi1) (sort_quantifiers psi2)
  | SSL.Forall (xs, psi) -> SSL.mk_forall (sort_vars xs) (sort_quantifiers psi)
  | SSL.Exists (xs, psi) -> SSL.mk_exists (sort_vars xs) (sort_quantifiers psi)

let rec unfold_quantifiers = function
  | SSL.Var v -> SSL.Var v
  | SSL.Pure term -> SSL.mk_pure term
  | SSL.Eq xs -> SSL.mk_eq_list xs
  | SSL.Distinct xs -> SSL.mk_distinct_list xs
  | SSL.PointsTo (x, ys) -> SSL.mk_pto_seq x ys
  | SSL.LS (x, y) -> SSL.mk_ls x y
  | SSL.And (psi1, psi2) -> SSL.mk_and [unfold_quantifiers psi1; unfold_quantifiers psi2]
  | SSL.Or (psi1, psi2) -> SSL.mk_or [unfold_quantifiers psi1; unfold_quantifiers psi2]
  | SSL.Not psi -> SSL.mk_not (unfold_quantifiers psi)
  | SSL.GuardedNeg (psi1, psi2) ->
      SSL.mk_gneg (unfold_quantifiers psi1) (unfold_quantifiers psi2)
  | SSL.Star psis -> SSL.mk_star (List.map unfold_quantifiers psis)
  | SSL.Septraction (psi1, psi2) ->
      SSL.mk_septraction (unfold_quantifiers psi1) (unfold_quantifiers psi2)
  | SSL.Forall (x :: xs, psi) ->
      unfold_bool `Forall x (SSL.mk_forall xs (unfold_quantifiers psi))
  | SSL.Exists (x :: xs, psi) ->
      unfold_bool `Exists x (SSL.mk_exists xs (unfold_quantifiers psi))

let apply_aux sl_graph phi =
  replace_iffs phi
  |> remove_iffs
  |> sort_quantifiers
  |> unfold_quantifiers

let rec apply_until_fixpoint sl_graph phi =
  let phi' = apply_aux sl_graph phi in
  if SSL.equal phi phi' then phi'
  else apply_until_fixpoint sl_graph phi'

let apply sl_graph = function
  | SSL.GuardedNeg (lhs, rhs) -> begin
      assert (SSL.is_negation_free lhs);
      assert (SSL.is_negation_free rhs);
      let lhs = apply_until_fixpoint sl_graph lhs in
      let rhs = apply_until_fixpoint sl_graph rhs in
      SSL.mk_gneg lhs rhs
    end
  | _ -> failwith "Broom preprocessor expects entailment"
