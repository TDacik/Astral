(*open SMT_sig

module type SET = sig

  include EQUALITY with type t = SMT.t

  val name : string
  (** Name of the encoding used for logging. *)

  (** Sorts *)

  val mk_sort : Sort.t -> Sort.t

  val get_elem_sort : t -> Sort.t

  (** Variables *)

  val mk_var : string -> Sort.t -> t

  val mk_fresh_var : string -> Sort.t -> t

  (** Construction of terms *)

  val mk_empty : Sort.t -> t

  val mk_singleton : t -> t

  val mk_enumeration : Sort.t -> t list -> t


  val mk_union : t list -> Sort.t -> t

  val mk_inter : t list -> Sort.t -> t

  val mk_diff : t -> t -> t

  val mk_compl : t -> t

  (** Predicates *)

  val mk_mem : t -> t -> t

  val mk_subset : t -> t -> t

  val mk_disjoint : t -> t -> t

  val mk_disjoint_list : t list -> t

  val mk_eq_empty : t -> t

  val mk_eq_singleton : t -> t -> t

  (** Accessors and syntactic checks *)

  val get_elems : t -> t list

  val may_disjoint : t -> t -> bool

end*)
