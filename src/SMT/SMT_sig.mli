open SMT

module type TERM = sig

  type term

  val mk_eq : t -> t -> t

  val mk_distinct : t list -> t

  val is_constant : t -> bool
  (** True if the value of the given term does not depend on a model in which it
      is evaluated. *)

  val identity : t -> t -> bool

  val get_sort : t -> Sort.t

  val map : (t -> t) -> t -> t

  val map_vars : (string -> SMT.Sort.t -> t) -> t -> t

end


module type SET = sig

  include TERM

  val name : string
  (** Name of the encoding used for logging. *)

  (** Sorts *)

  val mk_sort : Sort.t -> Sort.t

  val get_elem_sort : Term.t -> Sort.t

  (** Variables *)

  val mk_var : string -> Sort.t -> t

  val mk_fresh_var : string -> Sort.t -> t

  (** Construction of terms *)

  val mk_empty : Sort.t -> Term.t

  val mk_singleton : Term.t -> Term.t

  val mk_enumeration : Sort.t -> Term.t list -> Term.t


  val mk_union : Term.t list -> Sort.t -> Term.t

  val mk_inter : Term.t list -> Sort.t -> Term.t

  val mk_diff : Term.t -> Term.t -> Term.t

  val mk_compl : Term.t -> Term.t

  (** Predicates *)

  val mk_mem : Term.t -> Term.t -> Term.t

  val mk_subset : Term.t -> Term.t -> Term.t

  val mk_disjoint : Term.t -> Term.t -> Term.t

  val mk_eq_empty : Term.t -> Term.t

  val mk_eq_singleton : Term.t -> Term.t -> Term.t

  (** Accessors and syntactic checks *)

  val get_elems : Term.t -> Term.t list

  val may_disjoint : Term.t -> Term.t -> bool

end
