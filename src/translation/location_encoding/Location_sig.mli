(* Signature of heap location encoding.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

module type LOCATIONS_BASE = sig

  type t

  val name : string
  (** Name of the encoding. Used only for debugging. *)

  val init : Bounds.t -> t


  (** {2 Accessors} *)

  val nil_const : t -> SMT.Term.t

  val get_sort : t -> Sort.t
  (** Get sort of all locations. *)

  val get_sort_encoding : t -> Sort.t -> SMT.Term.t
  (** Get set representing SMT encoding of the specified sort. *)

  val get_constants : t -> SMT.Term.t list
  (** Return list of all locations. *)

  val get_constants_s : t -> Sort.t -> SMT.Term.t list
  (** Return list of all locations of the specified sort. *)

  (** {2 Inverse translation} *)

  val inverse_translate : t -> SMT.Model.model -> SMT.Term.t -> StackHeapModel.Location.t

  (** {2 Additional} *)

  val heap_axioms : t -> SMT.Term.t -> SMT.Term.t

  val lemmas : t -> SMT.Term.t

  (** {2 Debuging} *)

  val internal_repr : t -> string

end

module type LOCATIONS = sig

  include LOCATIONS_BASE

  (** {2 Declaration} *)

  val mk_var : t -> string -> SMT.Term.t
  (** Create location variable. *)

  val mk_fresh_var : t -> string -> SMT.Term.t
  (** Create fresh location variable. *)

  val mk_set_var : t -> string -> SMT.Term.t
  (** Create location set variable. *)

  val mk_fresh_set_var : t -> string -> SMT.Term.t
  (** Create fresh location set variable. *)


  (** {2 Translation} *)

  val translate_var : t -> SSL.t -> SMT.Term.t

  (** {2 Typing} *)

  val mk_of_type : t -> SMT.Term.t -> Sort.t -> SMT.t

  val mk_set_of_type : t -> SMT.Term.t -> Sort.t -> SMT.t

  (** {2 Quantifiers} *)

  val mk_exists : t -> (SMT.Term.t -> SMT.Term.t) -> SMT.t

  val mk_forall : t -> (SMT.Term.t -> SMT.Term.t) -> SMT.t

  (** {2 Axioms} *)

  val axioms: t -> SSL.t -> SMT.Term.t


  val set_sort : t -> Sort.t
  (** Sort of location sets. *)

  val powerset : t -> SMT.Term.t list list

end
