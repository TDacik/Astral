(* Methods for second-order quantifier elimination
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT
open Translation_context

(** Rewrite using first-order quantifiers over sets. **)
let no_elim _ phi =
  SMT.Term.map
    (fun t -> match t with
      | Exists2 (xs, _, phi) -> Exists (xs, phi)
      | Forall2 (xs, _, phi) -> Forall (xs, phi)
      | t -> t
    ) phi

(** Rewrite using extensive enumeration. *)

let enumerate_footprints xs context =
  List.map (fun _ ->
    Translation_context.locations_powerset context
    |> List.map (Set.mk_enumeration context.fp_sort)
  ) xs

let enumerate connective phi x range =
  range
  |> List.map (SMT.Term.substitute phi x)
  |> connective

let enumeration context phi =
  SMT.Term.map
    (fun t -> match t with
      | Exists2 (xs, None, phi) ->
        BatList.fold_left2 (enumerate Boolean.mk_or) phi xs (enumerate_footprints xs context)
      | Forall2 (xs, None, phi) ->
        BatList.fold_left2 (enumerate Boolean.mk_and) phi xs (enumerate_footprints xs context)
      | Exists2 (xs, Some ranges, phi) ->
        BatList.fold_left2 (enumerate Boolean.mk_or) phi xs ranges
      | Forall2 (xs, Some ranges, phi) ->
        BatList.fold_left2 (enumerate Boolean.mk_and) phi xs ranges
      | t -> t
    ) phi
