(* Signature for a generic translation
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

module type SET_ENCODING = sig

  include SMT_sig.SET
  (** Syntactic functions over set terms. *)

  val name : string
  (** Name of the set encoding. *)

  val rewrite : SMT.Term.t -> SMT.Term.t

  val rewrite_back : SMT.Term.t -> SMT.Model.model -> SMT.Model.model

end


module type LOCATIONS = sig

  type t

  val name : string

  val mk : string -> int -> t
  (** Create location sort of cardinality n. *)

  val get_sort : t -> Sort.t

  val mk_var : string -> Sort.t -> SMT.Term.t

  val mk_fresh_var : string -> Sort.t -> SMT.Term.t

  val get_constants : t -> SMT.Term.t list

  val var_axiom : t -> SMT.Term.t -> SMT.Term.t

  (* Quantification *)

  val mk_forall : Sort.t -> (SMT.Term.t -> SMT.Term.t) -> SMT.Term.t

  val mk_exists : Sort.t -> (SMT.Term.t -> SMT.Term.t) -> SMT.Term.t

  (* Additional *)

  val location_lemmas : t -> SMT.Term.t

end

type local_bound := int * int

module type LIST_ENCODING = sig

  val semantics :
    Translation_context.t ->
    SMT.Term.t ->
    SMT.Term.t ->
    SMT.Term.t ->
    local_bound ->
    SMT.Term.t
  (** semantics context x y fp *)

  val axioms :
    Translation_context.t ->
    SMT.Term.t ->
    SMT.Term.t ->
    SMT.Term.t ->
    local_bound ->
    SMT.Term.t
  (** axioms context x y fp *)

  val footprints :
    Translation_context.t ->
    SMT.Term.t ->
    SMT.Term.t ->
    SMT.Term.t ->
    local_bound ->
    SMT.Term.t

end

(** Base encoding groups together encoding of sets and locations. *)
module type BASE_ENCODING = sig
  module Set : SET_ENCODING
  module Locations : LOCATIONS
end

(** Predicate encoding groups together encoding of inductive predicates. *)
module type PREDICATE_ENCODING = sig
  module ListEncoding : LIST_ENCODING
end

module type ENCODING = sig
  include BASE_ENCODING
  module ListEncoding : LIST_ENCODING
end
