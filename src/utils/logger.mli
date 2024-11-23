(* Interface for printing and debugging
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

open Logger_sig

val warning : ('a, Format.formatter, unit) format -> 'a

val error : ('a, Format.formatter, unit) format -> 'a

module Make (C : CONFIG) : LOGGER

module MakeWithDir (C : CONFIG_WITH_DIR) : LOGGER_WITH_DIR

module MakeWithDump (C : CONFIG_WITH_DUMP) : LOGGER_WITH_DUMP with type t = C.t
(** Extension of {!MakeWithDir} by a function to dump a object. This is usefull
    for graphs, for which we do not have a show function. *)
