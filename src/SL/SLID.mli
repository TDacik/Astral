(* Separation logic with inductive definitions.
 *
 * Those functions depends on built-in/user-defined inductive predicates and cannot
 * be implemented directly in SL module.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

val has_unique_footprint : SL.t -> bool

val has_unique_shape : SL.t -> bool

val has_user_defined_predicates : SL.t -> bool
