(* Parser for smtlib2 format
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

val parse : string -> SSL.t * SSL.Variable.t list
(** Parse a smtlib file given by path *)
