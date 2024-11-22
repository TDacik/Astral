(* Representation of logic sorts.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

open Datatype_sig

type t =
  | Bool
  | Int
  | Finite of Identifier.t * string list
  | Set of t
  | Sequence of t
  | Array of t * t
  | Bitvector of int
  | Tupple of t list
  | Sum of t list
  | Uninterpreted of Identifier.t
  | Loc of Identifier.t * Identifier.t list

include PRINTABLE with type t := t
include COMPARABLE with type t := t
include COLLECTIONS with type t := t

val name : t -> string

val all_names : t -> string list
(** In case of location sort, return its name and all defined aliases. For other sorts,
    return a singleton list with a name. *)

val cardinality : t -> int option
(** For finite sorts, return their cardinality *)

(** {2 Built-in locations sorts} *)

val loc_ls : t

val loc_nil : t

(** {2 Constructors} *)

val int : t

val bool : t

val mk_finite : string -> string list -> t

val mk_loc : ?aliases:string list -> string -> t

val mk_set : t -> t

val mk_uninterpreted : string -> t

val mk_array : t -> t -> t

val mk_bitvector : int -> t

val mk_sum : t list -> t

(** {2 Predicates} *)

val is_bool : t -> bool

val is_loc : t -> bool

val is_set : t -> bool

val is_array : t -> bool

val is_bitvector : t -> bool

val is_nil : t -> bool

val get_dom_sort : t -> t
(** Get domain sort of either set, sequence or array sort. *)

val get_range_sort : t -> t
(** Get range sort of an array sort. *)

val get_width : t -> int
(** Get width of a bitvector sort. *)

val get_constant_names : t -> string list
(** Get names of constants of a finite sort. *)

val is_atomic : t -> bool
(** Atomic sorts are Bool, Int, Finite, Bitvector and Loc. *)

(** {2 Operations} *)

val substitute : t -> t -> t -> t
(** [substitute sort pattern target] replaces all occurences of sort pattern by atomic sort
    target. *)

(** SMTLIB *)

val to_smt2_decl : t -> string
