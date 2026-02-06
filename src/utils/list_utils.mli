(* List utilities
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

val min_cmp : cmp:('a -> 'a -> int) -> 'a list -> 'a

val max_cmp : cmp:('a -> 'a -> int) -> 'a list -> 'a

val index_of_cmp : ('a -> 'a -> int) -> 'a -> 'a list -> int option

val all_equal : ('a -> 'a -> bool) -> 'a list -> bool
(** Check whether all elements of list are equal w.r.t. supplied equality function. *)

val all_distinct : ('a -> 'a -> bool) -> 'a list -> bool
(** Check whether all elements of list are distinct w.r.t. supplied equality function. *)

val diagonal_product : 'a list -> ('a * 'a) list

val for_all2' : ('a -> 'a -> bool) -> 'a list -> bool

val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
(** Convert list of tripples to a tripple of lists *)

val map3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list

val zip3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list

val sublists : 'a list -> 'a list list


(** {2 Relations as lists} *)

module Relation (X : Datatype_sig.COMPARISON) : sig

  type 'a relation := ('a * 'a) list

  val transitive_closure : X.t relation -> X.t relation

  val transitive_closure_list : X.t list list -> X.t relation
  (** Given list of equivalence classes, compute the transitive closure. *)

end
