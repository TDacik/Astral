(* Solver's public API.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

type solver

val init :
  ?timeout : Int.t ->
  ?backend : Config.Backend.t ->
  ?encoding : Config.Encoding.t ->
  ?quantifier_encoding : Config.QuantifierEncoding.t ->
  ?produce_models : bool ->
  ?use_builtin_defs : bool ->
  ?dump_queries : [`None | `Full of string] ->
  ?source : string ->
  unit -> solver
(** Create a solver object:

    @param timeout Timeout in seconds. When the given limit is reached, return unknown.
    @param backend Backend SMT solver
    @param encoding Strategy for encoding
    @param use_builtins_defs Use builtin sorts, structures and inductive definitions
    @param dump_queries Store queries in directory given by the path. *)

val solve : ?timeout:Int.t -> solver -> SL.t -> [ `Sat of StackHeapModel.t option | `Unsat | `Unknown of string ]
(** Check satisfiability of a formula.

    @param timeout Timeout in seconds. When the given limit is reached, return unknown.
                   When used, has higher priority than limit set by solver initialisation. *)

val set_heap_sort : HeapSort.t -> solver -> solver

val add_heap_sort : HeapSort.t -> solver -> solver


val query_id: unit -> int

val dump_stats : solver -> unit

(** {2 Queries} *)

exception UnknownResult of string
(** Exception raised when the result of query is not known (e.g., because of timeout). *)

val check_sat : solver -> SL.t -> bool
(** [check_sat solver F] checks satisfiability of F. *)

val check_entl : solver -> SL.t -> SL.t -> bool
(** [check_entl solver F G] checks validity of the entailment F |= G. *)

val check_equiv : solver -> SL.t -> SL.t -> bool
(** [check_equiv solver F G] checks equivalence of F and G. *)
