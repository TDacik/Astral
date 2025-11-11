(* Utilities for solver binary.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

val print_error : ('a, Format.formatter, unit) format -> 'a
(** Print an error message, but do not exit. *)

val user_error : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** Print an error message and exit with return code 2. *)

val internal_error : ?backtrace:bool -> exit_code:int -> string -> _
