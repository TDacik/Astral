(* Z3 adapter for Astral
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

type t = Z3.Expr.expr

let context = ctx

(** Translation of SMT expressions *)
let translate = function
  | Variable (x, _) -> failwith ""

  | True -> Boolean.mk_true context
  | False -> Boolean.mk_false context
  | And es -> Boolean.mk_and context es
  | Or es -> Boolean.mk_or context es
  | Not e -> Boolean.mk_not context e
  | Implies (e1, e2) -> Boolean.mk_implies context e1 e2

  | Union (e1, e2) -> Set.mk_union context [e1; e2]
  | Inter (e1, e2) -> Set.mk_inter context [e1; e2]


let solve = ()
