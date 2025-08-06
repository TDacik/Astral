(* High-level access to command-line options.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Backend_sig
open Translation_sig

include (sig
  include module type of struct include Options_base end
end) (** @inline *)

type backend = [`Bitwuzla | `CVC5 | `Z3 | `Auto ]
type encoding = [`Bitvectors | `Sets]
type quantifier_encoding = [`Direct | `Enum]

val parse : version:string -> string

val backend : unit -> (module BACKEND)
val set_backend : backend -> unit

val incremental_backend : unit -> (module BACKEND)

val encoding : unit -> (module ENCODING)
val set_encoding : encoding -> unit

val set_quantifier_encoding : quantifier_encoding -> unit

val set_produce_models : bool -> unit

val check : unit -> unit
