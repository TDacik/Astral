(* Encoding of bounded reachability.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT
open Translation_context

(* ==== Basic predicates ==== *)

(** Create a term representing successor of the given location *)
let mk_succ ctx loc = Array.mk_select ctx.heap loc

(** Create a term representing n-th successor of the given location *)
let mk_nth_succ ctx loc n =
  List.init n (fun _ -> 0)
  |> List.fold_left (fun term _ -> mk_succ ctx term) loc

(** Create a term representing equality of x with n-th successor of the given location. *)
let mk_nth_succ_eq ctx loc x n =
  let select = mk_nth_succ ctx loc n in
  Boolean.mk_eq select x

(** Create a term representing that the given location is allocated. *)
let mk_is_allocated fp loc = Set.mk_mem loc fp

(** Create a term representing that the given location is not allocated. *)
let mk_is_not_allocated fp loc = Boolean.mk_not (mk_is_allocated fp loc)


(* ==== Reachability and paths ==== *)

(** Location x can reach y in exactly n steps *)
let reach_n ctx x y n = mk_nth_succ_eq ctx x y n

(** Location x can reach y in [min, max] steps *)
let reach ctx x y (min, max) =
  BatList.range min `To ctx.location_bound
  |> List.map (reach_n ctx x y)
  |> Boolean.mk_or

(** Create a term representing set of all reachable locations in at most n steps *)
let all_reachable_n ctx fp x n =
  if n = 0 then Set.mk_eq_empty fp
  else
    BatList.range 0 `To (n - 1)
    |> List.map (mk_nth_succ ctx x)
    |> Set.mk_enumeration ctx.fp_sort
    |> Set.mk_eq fp

(** Set fp is equal to the domain of a path from x to y of length n *)
let path_n ctx fp x y n =
  let phi1 = reach_n ctx x y n in
  let phi2 = all_reachable_n ctx fp x n in
  let phi3 = Boolean.mk_not (Set.mk_mem y fp) in
  Boolean.mk_and [phi1; phi2; phi3]

(** Set fp is equal to the domain of a simple path from x to y, or empty if such a path does not
    exist. *)
let path ctx fp x y (min, max) =
  let paths =
      BatList.range min `To max
      |> List.map (path_n ctx fp x y)
      |> Boolean.mk_or
    in
    let no_path = Boolean.mk_not (reach ctx x y (min, max)) in
    let empty = Set.mk_eq_empty fp in
    let no_path_empty = Boolean.mk_and [no_path; empty] in
    Boolean.mk_or [paths; no_path_empty]

(** Functional version *)

let all_reachable_n_term ctx x y n =
  if n = 0 then Set.mk_empty ctx.fp_sort
  else
    BatList.range 0 `To (n - 1)
    |> List.map (mk_nth_succ ctx x)
    |> Set.mk_enumeration ctx.fp_sort

let rec path_term ctx x y (min, max) =
  if min > max then (all_reachable_n_term ctx x y min)
  else Boolean.mk_ite
    (reach_n ctx x y min)
    (all_reachable_n_term ctx x y min)
    (path_term ctx x y (min + 1, max))
