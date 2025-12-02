(* JSON output of solver's result
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

val print : Context.t -> unit
(** Print json representation of results to standard output. *)

val output : string -> Context.t -> unit
(** Write results to a json file given by path *)
