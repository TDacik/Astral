(* Parser for smtlib2 format
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

val get_status : string -> string

val parse : string -> Input.t
(** Parse a smtlib file given by path *)
