(* Quantifier prefix
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT

module List = BatList

module Quantifier = struct

  type range =
    | All
    | Range of SMT.term list

  type t =
    | Forall of SMT.term * range
    | Exists of SMT.term * range

  let get_range = function Forall (_, r) -> r | Exists (_, r) -> r

  let mk_forall x = Forall (x, All)
  let mk_exists x = Exists (x, All)
  let mk_forall_range x r = Forall (x, Range r)
  let mk_exists_range x r = Exists (x, Range r)

  let negate = function
    | Forall (x, r) -> Exists (x, r)
    | Exists (x, r) -> Forall (x, r)

  let is_universal = function
    | Forall _ -> true
    | Exists _ -> false

  let is_existential q = not @@ is_universal q

  let apply phi = function
    | Forall (x, All) -> SMT.Quantifier.mk_forall x phi
    | Exists (x, All) -> SMT.Quantifier.mk_exists x phi

    (* Bounded quantifiers *)
    | Forall (x, Range range) ->
       let pre = Boolean.mk_and (List.map (fun r -> Boolean.mk_eq r x) range) in
       let phi = Boolean.mk_implies pre phi in
       Quantifier.mk_forall x phi
    | Exists (x, Range range) ->
       let pre = Boolean.mk_or (List.map (fun r -> Boolean.mk_eq r x) range) in
       let phi = Boolean.mk_implies pre phi in
       Quantifier.mk_exists x phi

  let show_range = function
    | All -> ""
    | Range terms ->
      List.map Term.show terms
      |> String.concat ", "

  let show = function
    | Forall (x, r) -> Format.asprintf "∀ %s in (%s)" (Term.show x) (show_range r)
    | Exists (x, r) -> Format.asprintf "∃ %s in (%s)" (Term.show x) (show_range r)

end

module QuantifierPrefix = struct

  type t = Quantifier.t list

  let negate = List.map Quantifier.negate

  let drop_implicit = List.drop_while Quantifier.is_existential

  let apply = List.fold_left Quantifier.apply

  let empty = []

  let prepend q qs = q :: qs

  let add qs q = List.append qs [q]

  let join qs1 qs2 = qs1 @ qs2

  let join_choice qs1 qs2 q ?(range=None) x =
    let q = match q, range with
      | `Forall, Some range -> Quantifier.Forall (x, Range range)
      | `Exists, Some range -> Quantifier.Exists (x, Range range)
      | `Forall, None -> Quantifier.Forall (x, All)
      | `Exists, None -> Quantifier.Exists (x, All)
    in
    qs1 @ qs2 @ [q]

  let show qs = String.concat ", " (List.map Quantifier.show qs)

end
