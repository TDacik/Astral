(* Signature for a generic translation
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

module type MINIMAL_SET = Set_sig.MINIMAL_SET

module type SET = Set_sig.SET

module type LOCATIONS = sig

  (* {3 Functions used to build Astral's context} *)

  val mk_sort : string -> int -> SMT.Sort.t

  val mk_const : SMT.Sort.t -> string -> SMT.Term.t

  val enumeration : SMT.Sort.t -> SMT.Term.t list

  (* {3 functions already using Astral's context} *)

  val var_to_expr : Context.t -> SSL.Variable.t -> SMT.Term.t

  val vars_to_exprs : Context.t -> SMT.Term.t list


  val location_lemmas : Context.t -> SMT.Term.t

  val exists : Context.t -> (SMT.Term.t -> SMT.Term.t) -> SMT.Term.t
  (** First-order universal quantifier. *)

  val forall : Context.t -> (SMT.Term.t -> SMT.Term.t) -> SMT.Term.t
  (** First-order universal quantifier. *)

  val exists2 : Context.t -> (SMT.Term.t -> SMT.Term.t) -> SMT.Term.t
  (** Second-order existential quantifier. *)

  val forall2 : Context.t -> (SMT.Term.t -> SMT.Term.t) -> SMT.Term.t
  (** Second-order universal quantifier. *)

end

type local_bound := int * int

module type LIST_ENCODING = sig

  val semantics :
    Context.t ->
    SMT.Term.t ->
    SMT.Term.t ->
    SMT.Term.t ->
    local_bound ->
    SMT.Term.t
  (** semantics context x y fp *)

  val axioms :
    Context.t ->
    SMT.Term.t ->
    SMT.Term.t ->
    SMT.Term.t ->
    local_bound ->
    SMT.Term.t
  (** axioms context x y fp *)

end

module type ENCODING = sig

  module Set : SET

  module Locations : LOCATIONS

  module ListEncoding : LIST_ENCODING

end
