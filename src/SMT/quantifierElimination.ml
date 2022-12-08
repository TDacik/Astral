(* Methods for second-order quantifier elimination
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT

(** Rewrite using first-order quantifiers over sets. **)
let no_elim phi =
  SMT.Term.map
    (fun t -> match t with
      | Exists2 (xs, _, phi) -> Exists (xs, phi)
      | Forall2 (xs, _, phi) -> Forall (xs, phi)
      | t -> t
    ) phi

(** Rewrite using extensive enumeration. *)

let enumerate connective phi x range =
  range
  |> List.map (SMT.Term.substitute phi x)
  |> connective

let enumeration phi =
  SMT.Term.map
    (fun t -> match t with
      | Exists2 (xs, ranges, phi) -> BatList.fold_left2 (enumerate Boolean.mk_or) phi xs ranges
      | Forall2 (xs, ranges, phi) -> BatList.fold_left2 (enumerate Boolean.mk_and) phi xs ranges
      | t -> t
    ) phi
