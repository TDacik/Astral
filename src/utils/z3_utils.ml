(* Z3 utilities
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Z3

let rec drop_quantifiers expr =
  let ast = Expr.ast_of_expr expr in
  if AST.is_quantifier ast then
    let q = Quantifier.quantifier_of_expr expr in
    drop_quantifiers (Quantifier.get_body q)
  else expr

(** Size of expression computed as number of nodes in its AST *)
let rec expr_size expr =
  let expr = drop_quantifiers expr in
  try 1 + (BatList.sum @@ List.map expr_size @@ Expr.get_args expr)
  with _ -> 1 (* free variable *)

let numeral_to_int expr =
  let name = Expr.to_string expr in
  try int_of_string @@ List.nth (String.split_on_char '|' name) 0
  with _ -> failwith ("Cannot convert numeral " ^ name)
