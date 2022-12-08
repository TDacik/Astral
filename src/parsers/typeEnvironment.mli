(* Simple type checking for SSL parsing
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

val declare : string -> Sort.t -> unit

val type_of : string -> Sort.t

val show : unit -> string
