(* Utilities for manipulation with vectors of bits.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

type width := int

type t = int * width [@@deriving compare, equal]

val width : t -> width

include Datatype_sig.PRINTABLE with type t := t

(** {2} Constructors *)

val of_int : int -> width -> t

val of_string : string -> t

val zero : width -> t

val one : width -> t

val full_zeros : width -> t

val full_ones : width -> t

(** {2} Operations *)

val nth : t -> int -> bool

(** {2} Conversions *)

val to_int : t -> int

val to_string : t -> string

val to_set : t -> t list
