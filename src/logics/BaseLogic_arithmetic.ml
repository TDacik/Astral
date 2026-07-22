(* Integer arithmetic.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open BaseLogic_terms
open BaseLogic_utils


(** Constructors without simplification *)
module Plain = struct
  include BaseLogic_equality.Plain

  let mk_var name = mk_var name Sort.int
  let mk_fresh_var name = mk_fresh_var name Sort.int

  let mk_const n = mk_constant (Constant.mk_int n)
  let zero = mk_const 0
  let one  = mk_const 1

end

module Smart = struct
  include Plain
  include BaseLogic_equality.Smart

  let mk_plus xs = mk_smart_app Plus ~neutral:zero xs
  let mk_minus lhs rhs = mk_app Minus [lhs; rhs]
  let mk_mult = mk_smart_app Mult ~neutral:one ~anihilator:zero

  let mk_lesser lhs rhs = mk_app Lesser [lhs; rhs]
  let mk_lesser_eq lhs rhs = mk_app LesserEqual [lhs; rhs]

end
