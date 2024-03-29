(* General utilities.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

val error : string -> _

val cmd_option_error : string -> string -> _

val internal_error : ?backtrace:bool -> string -> _
(** Fail with message. *)
