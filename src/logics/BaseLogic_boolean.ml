(* Boolean logic.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open BaseLogic_terms
open BaseLogic_utils

module Smart = struct
  include BaseLogic_booleanValues
  include BaseLogic_equality.Smart

  let mk_var name = mk_var name Sort.bool
  let mk_fresh_var name = mk_fresh_var name Sort.bool

  (** We can never simplify pure(phi) /\ emp ~> pure(phi) to be able
      to represent formulae in imprecise semantics.

      TODO: is this still true?*)
  let mk_and = mk_smart_app And ~neutral:tt ~anihilator:ff

  let mk_or = mk_smart_app Or ~neutral:ff ~anihilator:tt

  (** Not valid for separation logic with precise semantics! *)
  let mk_not = function
    | Application (Equal, [x; y]) -> mk_distinct [x; y]
    | Application (Distinct, [x; y]) -> mk_eq [x; y]
    | other -> mk_app Not [other]

  let mk_implies lhs rhs = match lhs, rhs with
    | lhs, rhs when equal lhs tt -> rhs
    | lhs, rhs when equal lhs ff -> tt
    | lhs, rhs when equal rhs ff -> mk_not lhs
    | lhs, rhs when equal rhs tt -> tt
    | lhs, rhs -> mk_app Implies [lhs; rhs]

  let mk_iff = mk_app Iff

  (* IfThenElse is a sequence [guard1, case1, guard2, case2, ..., else].
     Such an representation is not nice, but it is hidden by view types. *)

  let mk_ite cond b_then b_else = match cond with
    | c when equal c tt -> b_then
    | c when equal c ff -> b_else
    | _ -> mk_app IfThenElse [cond; b_then; b_else]

  let rec mk_multiple_ite cases t_else = match cases with
    | (c, t) :: rest -> mk_ite c t (mk_multiple_ite rest t_else)
    | [] -> t_else

  (** Some syntax sugar *)
  let mk_and2 x y = mk_and [x; y]
  let mk_or2  x y = mk_or [x; y]

end
