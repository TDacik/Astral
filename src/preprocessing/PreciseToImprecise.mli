(* Conversion between precise and imprecise semantics of (dis-)equalities.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

val to_precise : SL.t -> SL.t
(** Converts input in precise semantics to equivalent formula in imprecise semantics.

    If input represents satisfiability/entailment of symbolic heaps, then result do as well. *)

val to_imprecise : SL.t -> SL.t
(** Converts input in imprecise semantics to equivalent formula in precise semantics.

    If input represents satisfiability/entailment of symbolic heaps, then result do as well. *)
