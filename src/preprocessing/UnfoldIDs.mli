
module BoundMap := SL.MonoMap(SL.Term.MonoList)
(** Mapping from predicates to list of dangling variables that could appear inside its unfolding. *)

val apply_ctx : Context.t -> Context.t
