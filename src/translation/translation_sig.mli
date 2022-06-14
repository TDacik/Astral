(* Signature for a generic translation
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

module type MINIMAL_SET = Set_sig.MINIMAL_SET

module type SET = Set_sig.SET

module type LOCATIONS = sig

  (* {3 Functions used to build Astral's context} *)

  val mk_sort : string -> int -> SMT.sort

  val mk_const : SMT.sort -> string -> SMT.term

  val enumeration : SMT.sort -> SMT.term list

  (* {3 functions already using Astral's context} *)

  val var_to_expr : Context.t -> SSL.Variable.t -> SMT.term

  val vars_to_exprs : Context.t -> SMT.term list


  val location_lemmas : Context.t -> SMT.term

  val exists : Context.t -> (SMT.term -> SMT.term) -> SMT.term
  (** First-order universal quantifier. *)

  val forall : Context.t -> (SMT.term -> SMT.term) -> SMT.term
  (** First-order universal quantifier. *)

  val exists2 : Context.t -> (SMT.term -> SMT.term) -> SMT.term
  (** Second-order existential quantifier. *)

  val forall2 : Context.t -> (SMT.term -> SMT.term) -> SMT.term
  (** Second-order universal quantifier. *)

end

type local_bound := int * int

module type LIST_ENCODING = sig

  val semantics :
    Context.t ->
    SMT.term ->
    SMT.term ->
    SMT.term ->
    local_bound ->
    SMT.term
  (** semantics context x y fp *)

  val axioms :
    Context.t ->
    SMT.term ->
    SMT.term ->
    SMT.term ->
    local_bound ->
    SMT.term
  (** axioms context x y fp *)

end

module type ENCODING = sig

  module Set : SET

  module Locations : LOCATIONS

  module ListEncoding : LIST_ENCODING

end
