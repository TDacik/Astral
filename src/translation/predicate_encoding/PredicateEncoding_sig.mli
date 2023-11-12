(* Signature of predicate encoding.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Context_sig

module type PREDICATE_BASE_ENCODING = sig

  module Context : CONTEXT

  val name : string

  val semantics :
    Context.t -> SMT.t list -> SMT.t list -> SMT.t -> PredicateBounds.Entry.t -> SMT.t
  (** [semantics ctx selectors vars domain] *)

  val footprints :
    Context.t -> SMT.t list -> SMT.t list -> SMT.t -> PredicateBounds.Entry.t -> SMT.t list
  (** [footprints] ctx selectors vars domain] *)

end


module type PREDICATE_ENCODING = sig

  include PREDICATE_BASE_ENCODING

  type predicate_translation := SMT.t * SMT.t * SMT.t list

  val translate : Context.t -> SSL.t -> SMT.t -> predicate_translation

end
