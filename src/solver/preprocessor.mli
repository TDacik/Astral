(* Top-level preprocessor.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

val remove_unused_elements : ?with_vars:bool -> Context.t -> Context.t

val first_phase : Context.t -> Context.t

val second_phase : Context.t -> Context.t
