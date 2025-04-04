(* Utilities for convertors.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let status_to_string = function
  | `Sat -> "sat"
  | `Unsat -> "unsat"
  | `Unknown -> "unknown"

exception NotSupported of string
