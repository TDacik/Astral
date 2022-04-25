(* Builder of set encoding from minimal signature
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Z3

open Translation_sig

module Make (Set : MINIMAL_SET) = struct

  include Set

  let mk_distinct context set1 set2 =
    Set.mk_eq context set1 set2
    |> Boolean.mk_not context

  let mk_eq_empty context set =
    let sort = get_elem_sort set in
    let empty = Set.mk_empty context sort in
    Set.mk_eq context set empty

  let mk_eq_universal context set =
    let sort = get_elem_sort set in
    let universe = Set.mk_universal context sort in
    Set.mk_eq context set universe

  let mk_eq_singleton context set elem =
    let singleton = Set.mk_singleton context elem in
    Set.mk_eq context set singleton

  let mk_are_disjoint context set1 set2 =
    mk_eq_empty context (Set.mk_inter context [set1; set2])

end
