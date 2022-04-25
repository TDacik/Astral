(* Quantifier prefix
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Z3

open Context

module List = BatList

module Quantifier = struct

  type t =
    | Forall of Expr.expr
    | Exists of Expr.expr

  let negate = function
    | Forall x -> Exists x
    | Exists x -> Forall x

  let is_universal = function
    | Forall _ -> true
    | Exists _ -> false

  let is_existential q = not @@ is_universal q

  let apply context phi q =
    let quantifier = match q with
      | Forall x -> Quantifier.mk_forall_const context.solver [x] phi None [] [] None None
      | Exists x -> Quantifier.mk_exists_const context.solver [x] phi None [] [] None None
    in
    Quantifier.expr_of_quantifier quantifier

  let show = function
    | Forall x -> Format.asprintf "∀ %s" (Expr.to_string x)
    | Exists x -> Format.asprintf "∃ %s" (Expr.to_string x)

end

module QuantifierPrefix = struct

  type t = Quantifier.t list

  let negate = List.map Quantifier.negate

  let drop_implicit = List.drop_while Quantifier.is_existential

  let apply context = List.fold_left (Quantifier.apply context)

  let empty = []

  let add qs q = List.append qs [q]

  let concat qs1 qs2 = qs1 @ qs2

  let show qs = String.concat ", " (List.map Quantifier.show qs)

end
