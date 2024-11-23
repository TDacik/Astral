(* Arithmetic on integer intervals.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

type t = Int.t * Int.t [@@deriving compare, equal]

include Datatype_sig.PRINTABLE with type t := t

val plus : t -> t -> t
(** Component-wise plus. *)

val minus : t -> t -> t
(** Component-wise truncated minus. *)

val min : int -> t -> t
(** [min x (a, b)] returns (min x a, min x b). *)

val max : int -> t -> t
(** [max x (a, b)] returns (max x a, max x b). *)


val join : t -> t -> t

val meet : t -> t -> t

val meet_list : t list -> t

val enum : t -> int list
(*Enumerate all integers in range given by the interval. *)
