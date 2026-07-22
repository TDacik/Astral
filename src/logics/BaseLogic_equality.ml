(* Equality.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open BaseLogic_terms
open BaseLogic_utils

module Boolean = BaseLogic_booleanValues

(** Constructors without simplification *)
module Plain = struct
  let mk_eq xs = mk_app Equal xs
  let mk_distinct xs = mk_app Distinct xs
end

module Smart = struct
  let mk_eq xs =
    check_same_type ~what:"=" xs;
    if !BaseLogic_config.do_simplification then match xs with
      | xs when List_utils.all_equal equal xs -> Boolean.tt
      | xs -> mk_app Equal xs
    else mk_app Equal xs

  let mk_distinct xs =
    check_same_type ~what:"distinct" xs;
    mk_app Distinct xs

  let mk_eq2 x y = mk_eq [x; y]
  let mk_distinct2 x y = mk_distinct [x; y]
end
