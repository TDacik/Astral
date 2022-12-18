type t =
  | Bool
  | Int
  | Finite of String.t * string list
  | Set of t
  | Sequence of t
  | Array of t * t
  | Bitvector of int
  | Loc

include Datatype_sig.PRINTABLE with type t := t
include Datatype_sig.COMPARABLE with type t := t

val get_dom_sort : t -> t
(** Get domain sort of either set, sequence or array sort. *)

val get_range_sort : t -> t
(** Get range sort of an array sort. *)

val get_width : t -> int
(** Get width of a bitvector sort. *)

val is_atomic : t -> bool
(** Atomic sorts are Bool, Int, Finite, Bitvector and Loc. *)

val is_set : t -> bool
