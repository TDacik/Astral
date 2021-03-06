(* Bounded unfolding of recursive predicates
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

val name : string

val unfold : SSL.t -> int -> SSL.t
(** Bounded unfolding of list-segment predicates *)

val convert : SSL.t -> string

val dump : string -> SSL.t -> unit
