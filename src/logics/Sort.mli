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
  | Loc of string
  | Tupple of t list
  | Sum of t list
  | Uninterpreted of string

include PRINTABLE with type t := t
include COMPARABLE with type t := t
include COLLECTIONS with type t := t

(** {2 Built-in locations sorts} *)

val loc_ls : t

val loc_dls : t

val loc_nls : t

val loc_nil : t

(** {2 Constructors} *)

val mk_loc : string -> t

val mk_uninterpreted : string -> t

val mk_array : t -> t -> t

val mk_sum : t list -> t

(** {2 Predicates} *)

val is_loc : t -> bool

val is_set : t -> bool

val is_array : t -> bool


val is_ls : t -> bool

val is_dls : t -> bool

val is_nls : t -> bool

val is_nil : t -> bool

val get_dom_sort : t -> t
(** Get domain sort of either set, sequence or array sort. *)

val get_range_sort : t -> t
(** Get range sort of an array sort. *)

val get_width : t -> int
(** Get width of a bitvector sort. *)

val is_atomic : t -> bool
(** Atomic sorts are Bool, Int, Finite, Bitvector and Loc. *)

(** {2 Operations} *)

val substitute : t -> t -> t -> t
(** [substitute sort pattern target] replaces all occurences of sort pattern by atomic sort
    target. *)
