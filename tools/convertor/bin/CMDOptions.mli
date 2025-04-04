(* Command-line options
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Convertor_sig

val convertor : unit -> (module CONVERTOR)

val imprecise : unit -> bool

val output_path : unit -> string

val input_name : unit -> string

val input_path : unit -> string

val debug : unit -> bool

val parse : unit -> string
