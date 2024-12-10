(* General utilities.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

val warning : ('a, Format.formatter, unit) format -> 'a

val error : ('a, Format.formatter, unit) format -> 'a

val cmd_option_error : string -> string -> _

val user_error : string -> _

val internal_error :
  ?backtrace:bool -> ?report:bool -> ?exit_code:int -> string -> _
(** Fail with message. *)
