(* Bounded unfolding of recursive predicates
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL
open Batteries

let name = "predicate unfolding"

(** Unfold ls(x,y) n times *)
let rec unfold_ls_n n x y = match n with
  | 0 -> Eq (x, y)
  | n ->
      let l = Variable.mk_fresh "l" in
      Star (Neq (x,y), Star (PointsTo (x, l), unfold_ls_n (n-1) l y))

let unfold_ls n x y =
  BatList.range 0 `To n
  |> BatList.map (fun n -> unfold_ls_n n x y)
  |> SSL.mk_or

(** Recursively apply unfolding on all subformulae *)
(** TODO: use some kind of fold/map? *)
let rec unfold phi n = match phi with
  | And (f1, f2) -> And (unfold f1 n, unfold f2 n)
  | Or (f1, f2) -> Or (unfold f1 n, unfold f2 n)
  | Not f -> Not (unfold f n)
  | GuardedNeg (f1, f2) -> GuardedNeg (unfold f1 n, unfold f2 n)
  | Star (f1, f2) -> Star (unfold f1 n, unfold f2 n)
  | Septraction (f1, f2) -> Septraction (unfold f1 n, unfold f2 n)
  | LS (x, y) -> unfold_ls n x y
  | atom -> atom

let convert phi n = Smtlib_convertor.translate_all (unfold phi n)

let dump file phi status n = Smtlib_convertor.dump file (unfold phi n) status
