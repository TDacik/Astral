(* High-level access to command-line options.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Backend_sig
open Translation_sig

type backend = [`Bitwuzla | `CVC5 | `Z3 | `Auto ]
type encoding = [`Bitvectors | `Sets]

val parse : unit -> string

val backend : unit -> (module BACKEND)
val set_backend : backend -> unit

val encoding : unit -> (module ENCODING)
val set_encoding : encoding -> unit

val check : unit -> unit
