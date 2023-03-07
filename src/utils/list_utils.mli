(* List utilities
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

val all_equal : ('a -> 'a -> bool) -> 'a list -> bool
(** Check whether all elements of list are equal w.r.t. supplied equality function. *)

val all_distinct : ('a -> 'a -> bool) -> 'a list -> bool
(** Check whether all elements of list are distinct w.r.t. supplied equality function. *)

val diagonal_product : 'a list -> ('a * 'a) list

val for_all2' : ('a -> 'a -> bool) -> 'a list -> bool

val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
(** Convert list of tripples to a tripple of lists *)

val map3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list
