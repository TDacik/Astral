(* Bounded unfolding of recursive predicates
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL

(** Unfold list segment predicates n times *)
let rec unfold_ls_n x y n = match n with
  | 0 -> SSL.mk_eq x y
  | 1 -> SSL.mk_star [SSL.mk_distinct x y; SSL.mk_pto x y]
  | n ->
      let l = SSL.mk_fresh_var "l" in
      Star (SSL.mk_distinct x y, Star (PointsTo (x, [l]), unfold_ls_n l y (n-1)))

(** Create disjunction of all unfolding up to length n *)
let unfold_ls x y n =
  BatList.range 0 `To n
  |> List.map (unfold_ls_n x y)
  |> SSL.mk_or

(** Recursively apply unfolding on all subformulae *)
let rec unfold phi n = match phi with
  | And (f1, f2) -> And (unfold f1 n, unfold f2 n)
  | Or (f1, f2) -> Or (unfold f1 n, unfold f2 n)
  | Not f -> Not (unfold f n)
  | GuardedNeg (f1, f2) -> GuardedNeg (unfold f1 n, unfold f2 n)
  | Star (f1, f2) -> Star (unfold f1 n, unfold f2 n)
  | Septraction (f1, f2) -> Septraction (unfold f1 n, unfold f2 n)
  | LS (x, y) -> unfold_ls x y n
  | atom -> atom
