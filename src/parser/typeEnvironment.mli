(* Simple type checking for SSL parsing
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

exception VariableRedefined of string

exception VariableNotDeclared of string

type t

val empty : t

val declare : t -> string -> Sort.t -> t

val type_of : t -> string -> Sort.t

val show : t -> string
