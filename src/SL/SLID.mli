(* Separation logic with inductive definitions.
 *
 * Those functions depends on built-in/user-defined inductive predicates and cannot
 * be implemented directly in SL module.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

val expected_sort_of_nil_in_atom : SL.t -> int -> Sort.t option
(** [expected_sort_of_nil_in_atom atom pos] returns the sort which should nil variable
    have at the position [pos] in [atom].

    @return None is returned if the sort cannot be uniquely determined. This is the case
            for atoms with determined boolean value such as nil = nil or nil |-> <...>.

    @raises Invalid argument if [atom] is not an atomic formula or [pos] is out of range
            of arguments of [atom]. *)

val has_unique_footprint : SL.t -> bool

val has_unique_shape : SL.t -> bool

val must_allocated_terms : SL.t -> SL.Term.t list

val may_dangling_terms : SL.t -> SL.Term.t list

val has_user_defined_predicates : SL.t -> bool

val get_inductive_definitions : ?original:bool -> SL.t -> InductiveDefinition.t list

val get_structs : SL.t -> MemoryModel.StructDef.t list
