(* Representation of solver's internal error.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

exception UnsupportedFragment of string * string
(** This exception is raised when unsupported fragment is detected. *)

exception UnknownResult of string * string

exception InternalError of Printexc.raw_backtrace * string * string
(** This exception represents an internal error. It should not
    be catched inside Astral library. *)

val unknown_result : ?details:string -> ('a, Format.formatter, unit, _) format4 -> 'a

val unsupported_fragment : ?details:string -> ('a, Format.formatter, unit, _) format4 -> 'a

val internal_error : ?details:string -> ('a, Format.formatter, unit, _) format4 -> 'a

val pretty_internal_error : ?trace:Printexc.raw_backtrace -> string -> details:string -> unit
