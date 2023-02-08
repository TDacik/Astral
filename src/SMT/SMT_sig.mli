open Logic_sig
open Datatype_sig

module type SMT_TERM = sig

  type t

  include PRINTABLE with type t := t
  include COMPARABLE with type t := t

  include LOGIC with type t := t
  (** Basic functions over logical formulae *)

  val mk_eq : t -> t -> t

  val mk_eq_list : t list -> t

  val mk_distinct : t -> t -> t

  val mk_distinct_list : t list -> t

  (* TODO: the following functions can be probably moved to LOGIC *)

  val map : (t -> t) -> t -> t

  val map_vars : (string -> Sort.t -> t) -> t -> t

end


module type SET = sig

  include SMT_TERM

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

  val mk_eq_empty : t -> t

  val mk_eq_singleton : t -> t -> t

  (** Accessors and syntactic checks *)

  val get_elems : t -> t list

  val may_disjoint : t -> t -> bool

end
