(* Arrays.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open BaseLogic_terms
open BaseLogic_utils

module Array = struct

  include BaseLogic_equality.Smart
  include BaseLogic_vars

  let mk_sort = Sort.mk_array

  let mk_const c dom_sort = mk_app (ConstArray dom_sort) [c]

  let mk_select arr index =
    check_type_prop ~what:"mk_select param0" ~expects:"array sort" Sort.is_array arr;
    check_type ~what:"mk_select param1" (Sort.get_dom_sort @@ get_sort arr) index;
    mk_app Select [arr; index]

  let mk_store arr index value = mk_app Store [arr; index; value]

  let rec mk_nary_select n arr index = match n with
    | 0 -> index
    | n -> mk_nary_select (n - 1) arr (mk_select arr index)

end
