(* Finite sets.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open BaseLogic_terms
open BaseLogic_utils

module Boolean = BaseLogic_booleanValues

module Sets = struct

  include BaseLogic_equality.Smart (* TODO: split *)
  include BaseLogic_vars

  (** Utility functions for constant sets *)

  let is_constant = function Application (Enum _, _) -> true | _ -> false

  let as_constant = function
    | Application (Enum _, xs) -> Set.of_list xs
    | set -> raise @@ Invalid_argument ("Not a constant set " ^ show set)

  let is_empty_constant term =
    try Set.is_empty @@ as_constant term
    with Invalid_argument _ -> false

  let mk_sort = Sort.mk_set

  let mk_constant sort =
    assert (Sort.is_set sort);
    mk_app (Enum sort)

  let mk_empty sort =
    assert (Sort.is_set sort);
    mk_constant sort []

  let mk_universe sort =
    assert (Sort.is_set sort);
    mk_app (Universe sort) []

  let mk_singleton elem =
    mk_constant (Sort.mk_set @@ get_sort elem) [elem]

  let mk_union sort =
    assert (Sort.is_set sort);
    mk_smart_app (Union sort) ~neutral:(mk_empty sort) ~anihilator:(mk_universe sort)

  let mk_inter sort =
    assert (Sort.is_set sort);
    mk_smart_app (Inter sort) ~neutral:(mk_universe sort) ~anihilator:(mk_empty sort)

  let mk_disjoint = mk_app Disjoint

  let mk_add set elem = mk_union (Sort.mk_set @@ get_sort elem) [set; mk_singleton elem]
  let mk_diff lhs rhs = mk_app Diff [lhs; rhs]
  let mk_compl set = mk_app Compl [set]

  let mk_mem elem set =
    if !BaseLogic_config.do_simplification && is_empty_constant set then Boolean.ff
    else mk_app Membership [elem; set]


  let mk_subset lhs rhs = mk_app Subset [lhs; rhs]
  let mk_eq_empty set = mk_eq [set; mk_empty @@ get_sort set]
  let mk_eq_singleton set elem = mk_eq [set; mk_singleton elem]

  let may_disjoint xs =
    try
      let xs = List.map as_constant xs in
      List_utils.diagonal_product xs
      |> List.for_all (fun (x, y) -> Set.disjoint x y)
    (* One of sets is symbolic *)
    with Invalid_argument _ -> true

  let get_elem_sort set = Sort.get_dom_sort @@ get_sort set

end
