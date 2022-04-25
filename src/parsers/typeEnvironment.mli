(* Simple type checking for SSL parsing
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

val declare : string -> SSL.Sort.t -> unit

val type_of : string -> SSL.Sort.t
