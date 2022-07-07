(* Incremental instantiation of quantifiers
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT
open Context

open Quantifiers
open Quantifier

(** Do not perform any elimination *)
let none phi qs = QuantifierPrefix.apply phi qs

(* === Elimination by explicit enumeration of possible instances === *)

(** Compute powerset of list *)
let rec powerset = function
  | [] -> [[]]
  | x :: xs ->
      let ps = powerset xs in
      ps @ List.map (fun s -> x :: s) ps

(** Returns all subsets of locations *)
let enumerate_sets context phi =
  powerset (context.locs)
  |> List.map (fun subset -> Set.mk_enumeration context.locs_sort subset)

let enumerate_heaps = []

let expand_one context phi q =
  let cons, acc, x, range = match q with
    | Forall (x, Range range) -> Boolean.mk_and, Boolean.mk_true (), x, range
    | Exists (x, Range range) -> Boolean.mk_or, Boolean.mk_false (), x, range
    | Forall (x, All) -> Boolean.mk_and, Boolean.mk_true (), x, enumerate_sets context phi
    | Exists (x, All) -> Boolean.mk_and, Boolean.mk_true (), x, enumerate_sets context phi

  in
  List.fold_left
    (fun acc v ->
      let instance = SMT.substitute phi x v in
      cons [acc; instance]
    ) acc range

let expand context phi qs = List.fold_left (expand_one context) phi qs
