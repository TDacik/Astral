(* Top-level preprocessor.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

val remove_unused_elements : ?with_vars:bool -> Context.t -> Context.t

val first_phase : Context.t -> Context.t

val second_phase : Context.t -> Context.t

(** The third phase is responsible for predicate unfolding. One can specify how
    dangling variables should be distributed between predicates on LHS. *)

module BoundMap := SL.MonoMap(SL.Term.MonoList)
(** Mapping from predicates to list of dangling variables that could appear inside its unfolding. *)

val third_phase : ?bound_map:BoundMap.t -> Context.t -> Context.t
