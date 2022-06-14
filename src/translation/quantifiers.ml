(* Quantifier prefix
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT

module List = BatList

module Quantifier = struct

  type range = SMT.term list

  type t =
    | Forall of SMT.term * range
    | Exists of SMT.term * range

  let negate = function
    | Forall (x, r) -> Exists (x, r)
    | Exists (x, r) -> Forall (x, r)

  let is_universal = function
    | Forall _ -> true
    | Exists _ -> false

  let is_existential q = not @@ is_universal q

  let apply context phi = function
      | Forall (x, range) ->
          let pre = Boolean.mk_and (List.map (fun r -> Boolean.mk_eq r x) range) in
          let phi = Boolean.mk_implies pre phi in
          Quantifier.mk_forall x phi
      | Exists (x, range) ->
          let pre = Boolean.mk_or (List.map (fun r -> Boolean.mk_eq r x) range) in
          let phi = Boolean.mk_implies pre phi in
          Quantifier.mk_exists x phi

  (*
  let apply context phi q =
    let quantifier = match q with
      | Forall x -> Quantifier.mk_forall_const context.solver [x] phi None [] [] None None
      | Exists x -> Quantifier.mk_exists_const context.solver [x] phi None [] [] None None
    in
    Quantifier.expr_of_quantifier quantifier
  *)

  let show_range range =
    List.map Term.to_string range
    |> String.concat ", "

  let show = function
    | Forall (x, r) -> Format.asprintf "∀ %s in (%s)" (Term.to_string x) (show_range r)
    | Exists (x, r) -> Format.asprintf "∃ %s in (%s)" (Term.to_string x) (show_range r)

end

module QuantifierPrefix = struct

  type t = Quantifier.t list

  let negate = List.map Quantifier.negate

  let drop_implicit = List.drop_while Quantifier.is_existential

  let apply context = List.fold_left (Quantifier.apply context)

  let empty = []

  let add qs q = List.append qs [q]

  let join qs1 qs2 = qs1 @ qs2

  let join_choice qs1 qs2 q x range =
    let q = match q with
      | `Forall -> Quantifier.Forall (x, range)
      | `Exists -> Quantifier.Exists (x, range)
    in
    qs1 @ qs2 @ [q]

  let show qs = String.concat ", " (List.map Quantifier.show qs)

end
