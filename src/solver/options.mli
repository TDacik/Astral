(* High-level access to command-line options.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Backend_sig
open Translation_sig

val parse : unit -> string

val backend : unit -> (module BACKEND)

val encoding : unit -> (module ENCODING)
