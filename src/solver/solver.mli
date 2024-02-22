(* Solver's public API.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

type solver

val init :
  ?backend : [`Z3 | `CVC5] ->
  ?encoding : [`Bitvectors | `Sets] ->
  ?produce_models : bool ->
  ?dump_queries : [`None | `Full of string] ->
  unit -> solver

val solve : solver -> SSL.t -> [ `Sat of StackHeapModel.t option | `Unsat | `Unknown of string ]

(** {2 Queries} *)

exception UnknownResult of string
(** Exception raised when the result of query is not known (e.g., because of timeout). *)

val check_sat : solver -> SSL.t -> bool
(** [check_sat solver F] checks satisfiability of F. *)

val check_entl : solver -> SSL.t -> SSL.t -> bool
(** [check_entl solver F G] checks validity of the entailment F |= G. *)

val check_equiv : solver -> SSL.t -> SSL.t -> bool
(** [check_equiv solver F G] checks equivalence of F and G. *)
