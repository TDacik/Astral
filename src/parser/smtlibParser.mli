(* Parser for SMT-LIB format
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

val parse : Context.t -> string -> Context.t
(** Parse a SMT-LIB body of separation logic formula given as a string. *)
