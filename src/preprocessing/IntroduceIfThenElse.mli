(* Introduction of the if-then-else operator instead of disjunctions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

val apply : ?forbidden_vars:SL.Variable.t list -> SL.t -> SL.t

val apply_ctx : ?forbidden_vars:SL.Variable.t list -> Context.t -> Context.t
