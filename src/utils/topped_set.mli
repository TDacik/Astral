
exception TopError

module Lift (Set : Set.S) : sig

  type t

  (** Constructors *)

  val top : t

  val empty : t

  val singleton : Set.elt -> t

  val of_list : Set.elt list -> t


  (** Convertors *)

  val elements : t -> Set.elt list


  val add : Set.elt -> t -> t

  val remove : Set.elt -> t -> t

  val cardinal : t -> int

  val choose : t -> Set.elt

  (** Binary operations *)

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  val apply_binop : (Set.elt -> Set.elt -> Set.elt option) -> t -> t -> t

  val cartesian_product : t -> t -> (Set.elt * Set.elt) list option

  (** Predicates *)

  val is_empty : t -> bool

  val equal : t -> t -> bool

end
