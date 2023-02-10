(* List utilities
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

val all_equal : ('a -> 'a -> bool) -> 'a list -> bool
(** Check whether all elements of list are equal w.r.t. supplied equality function. *)

val all_distinct : ('a -> 'a -> bool) -> 'a list -> bool
(** Check whether all elements of list are distinct w.r.t. supplied equality function. *)

val diagonal_product : 'a list -> ('a * 'a) list
