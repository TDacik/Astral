open MemoryModel

type t

include Datatype_sig.PRINTABLE with type t := t

val empty : t
(** Create an empty heap sort. *)

val union : t list -> t

val restriction : Sort.t list -> t -> t

val of_list : (Sort.t * StructDef.t) list -> t

val to_list : t -> (Sort.t * StructDef.t) list

val find_target : Sort.t -> t -> StructDef.t
(** Find target struct for a location sort. *)

val find_target_unwrapped : Sort.t -> t -> Sort.t
(** Find target sort for a classical sort wrapped into struct. *)

val is_loc_sort : t -> Sort.t -> bool

val get_loc_sorts : t -> Sort.t list

val get_structures : t -> StructDef.t list

val get_fields : t -> Field.t list
(** Get all fields in all target structures. *)

(* {2 Predicates} *)

val is_bitvector_model : t -> bool
(** True iff the given heap sort represents mappings from bitvectors to bitvectors,
    i.e., corresponds to low-level memory model. *)
