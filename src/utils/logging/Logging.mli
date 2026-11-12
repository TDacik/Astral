(* Interface for logging and debugging.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022-2026 *)

open Logging_sig

(** Basic logging functions without instantiation. *)

val warning : ('a, Format.formatter, unit) format -> 'a

module Make (_ : CONFIG) : LOGGER

module WithOutput (_ : CONFIG_WITH_PATH) : LOGGER_WITH_OUTPUT
