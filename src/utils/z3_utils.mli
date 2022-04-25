(* Z3 utilities
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Z3

val expr_size : Expr.expr -> int
(** Size of expression computed as number of nodes in its AST *)

val numeral_to_int : Expr.expr -> int
(** Convert numeral expression to an integere *)
