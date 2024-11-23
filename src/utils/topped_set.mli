
module Lift (Set : Set.S) : sig

  exception TopError

  type t

  (** Constructors *)

  val top : t

  val empty : t

  val singleton : Set.elt -> t

  val of_list : Set.elt list -> t

  val is_concrete : t -> bool

  (** Convertors *)

  val elements : t -> Set.elt list


  val add : Set.elt -> t -> t

  val remove : Set.elt -> t -> t

  val cardinal : t -> int

  val cardinal_opt : t -> int option

  val choose : t -> Set.elt

  (** Binary operations *)

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  val map : (Set.elt -> Set.elt) -> t -> t

  val apply_partial_binop : (Set.elt -> Set.elt -> Set.elt option) -> t -> t -> t

  val apply_partial_variadic_op : (Set.elt list -> Set.elt option) -> t list -> t

  val cartesian_product : t -> t -> (Set.elt * Set.elt) list option

  (** Predicates *)

  val is_empty : t -> bool

  val equal : t -> t -> bool

end
