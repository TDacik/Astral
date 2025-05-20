type t = {
  id : InductiveDefinition.t;
  root : SL.Variable.t;
  allocated : SL.Variable.t list; (** Variables allocated in all non-empty models *)
  never_allocated : SL.Variable.t list;

  skeleton_fields: MemoryModel.Field.t list;

  stable_size : int;
  fixpoint_size : float;
  unfolding_depth : int;
}

val show : t -> string

val get_root : ?params:SL.Term.t list -> t -> SL.Term.t
(** Return the root of the inductive definition. If params is not provided, result
    corresponds to the formal variables. *)

val get_holes : ?params:SL.Term.t list -> t -> SL.Term.t list

val get_must_allocated : ?params:SL.Term.t list -> t -> SL.Term.t list

val may_allocated : ?params:SL.Term.t list -> t -> SL.Term.t list

module M : Datatype_sig.MONO_MAP
  with type key := InductiveDefinition.t
   and type data := t
