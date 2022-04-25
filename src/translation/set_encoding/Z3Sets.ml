(* Set encoding based on Z3 sets
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Z3

module Minimal = struct

  module Set = Z3.Set
  include Set

  let mk_const_s context str sort =
    Expr.mk_const_s context str (Set.mk_sort context sort)

  let mk_fresh_const context str sort =
    Expr.mk_fresh_const context str (Set.mk_sort context sort)

  let get_elem_sort set = Z3Array.get_domain @@ Expr.get_sort set

  let mk_eq context s1 s2 = Boolean.mk_eq context s1 s2

  let mk_singleton context elem =
    Set.mk_set_add context (Set.mk_empty context (Expr.get_sort elem)) elem

  let mk_enumeration context sort elems =
    List.fold_left
      (fun set elem ->
        Set.mk_set_add context set elem
      ) (Set.mk_empty context sort) elems

  let mk_universal = Set.mk_full

  let mk_inter = Set.mk_intersection

  let mk_mem = Set.mk_membership

  let inverse_translation context model set =
    let elem_sort = Z3Array.get_domain @@ Expr.get_sort set in
    let elems = Enumeration.get_consts elem_sort in
    List.fold_left
    (fun acc elem ->
      match Model.eval model (mk_mem context elem set) false with
      | Some res ->
          if Boolean.is_true res then elem :: acc
          else acc
      | None -> failwith "Internal error"
    ) [] elems

end

(** Build and include full implementation of set *)
include SetBuilder.Make(Minimal)
