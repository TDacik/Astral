(* Representation of solver's internal error.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

exception Unsat of string

exception UnsupportedFragment of string * string
(** This exception is raised when unsupported fragment is detected. *)

exception UnknownResult of string * string

exception InternalError of Printexc.raw_backtrace * string * string
(** This exception represents an internal error. It should not
    be catched inside Astral library. *)

exception CmdOptionError of string * string * string

val unknown_result : reason:string -> ?details:string -> _

val unsupported_fragment : reason:string -> ?details:string -> _

val internal_error : reason:string -> ?details:string -> _

val cmd_option_error : param:string -> bad_value:string -> expected_values:string -> _

val pretty_internal_error : ?trace:Printexc.raw_backtrace -> string -> details:string -> unit
