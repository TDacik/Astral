(* Representation of solver's internal error.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

exception UnsupportedFragment of string * string
(** This exception is raised when unsupported fragment is detected. *)

exception UnknownResult of string * string

exception InternalError of string * string
(** This exception represents an internal error. It should not
    be catched inside Astral library. *)

exception CmdOptionError of string

val unknown_result : reason:string -> ?details:string -> _

val unsupported_fragment : reason:string -> ?details:string -> _

val internal_error : reason:string -> ?details:string -> _

val pretty_internal_error : string -> string -> unit
