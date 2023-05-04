(* Representation of logic sorts.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

open Datatype_sig

type t =
  | Bool
  | Int
  | Finite of String.t * string list
  | Set of t
  | Sequence of t
  | Array of t * t
  | Bitvector of int
  | Loc
  | Tupple of t list
  | Sum of t list
  | Uninterpreted of string

include PRINTABLE with type t := t
include COMPARABLE with type t := t
include COLLECTIONS with type t := t

val mk_uninterpreted : string -> t

(** {2 Constructors of non-atomic sorts *)

val mk_array : t -> t -> t


val get_dom_sort : t -> t
(** Get domain sort of either set, sequence or array sort. *)

val get_range_sort : t -> t
(** Get range sort of an array sort. *)

val get_width : t -> int
(** Get width of a bitvector sort. *)

val is_atomic : t -> bool
(** Atomic sorts are Bool, Int, Finite, Bitvector and Loc. *)

val is_set : t -> bool

(** {2 Operations} *)

val substitute : t -> t -> t -> t
(** [substitute sort pattern target] replaces all occurences of sort pattern by atomic sort
    target. *)
