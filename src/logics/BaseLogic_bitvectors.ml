(* Bitvectors.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open BaseLogic_terms
open BaseLogic_utils

module Bitvector = struct

  include BaseLogic_equality.Smart
  include BaseLogic_vars

  let mk_sort width = Sort.mk_bitvector width

  let mk_const bv = mk_app (Constant (Constant.mk_bitvector bv)) []
  let mk_const_of_int i width = mk_const @@ Bitvector.of_int i width
  let mk_const_of_string str = mk_const @@ Bitvector.of_string str

  let get_width bv = Sort.get_width @@ get_sort bv

  let mk_zero width = mk_const @@ Bitvector.zero width
  let mk_one width = mk_const @@ Bitvector.one width
  let mk_full_zeros width = mk_const @@ Bitvector.full_zeros width
  let mk_full_ones width = mk_const @@ Bitvector.full_ones width

  (* TODO: simplify *)
  let mk_bit_check bv index = mk_app BitCheck [bv; index]

  let mk_not bv = mk_app BitNot [bv]

  let mk_plus width xs =
    let neutral = mk_full_ones width in
    mk_smart_app ~neutral (BitPlus width) xs

  let mk_and width =
    let neutral = mk_full_ones width in
    let anihilator = mk_full_zeros width in
    mk_smart_app ~neutral ~anihilator (BitAnd width)

  let mk_or width =
    let neutral = mk_full_zeros width in
    let anihilator = mk_full_ones width in
    mk_smart_app ~neutral ~anihilator (BitOr width)

  let mk_xor width = mk_app (BitXor width)
  let mk_implies lhs rhs = mk_app BitImplies [lhs; rhs]
  let mk_compl bv = mk_app BitCompl [bv]

  let mk_shift_left bv shift = mk_app BitShiftLeft [bv; shift]
  let mk_shift_right bv shift = mk_app BitShiftRight [bv; shift]

  let mk_lesser bv1 bv2 = mk_app BitUnsignedLesser [bv1; bv2]
  let mk_lesser_eq bv1 bv2 = mk_app BitUnsignedLesserEqual [bv1; bv2]

end
