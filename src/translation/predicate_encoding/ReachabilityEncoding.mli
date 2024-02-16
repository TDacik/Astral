(* Encoding of bounded reachability.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Context_sig

module Make (Context : CONTEXT) : sig

  type bounds := int * int
  (** Type alias for path bounds. *)

  val reach : SMT.Term.t -> SMT.Term.t -> SMT.Term.t -> bounds -> SMT.Term.t
  (** [reach selector source target bounds] *)

  val path : Context.t -> SMT.Term.t -> SMT.Term.t -> SMT.Term.t -> bounds -> SMT.Term.t
  (** [path context selector source target bounds] *)

  val nested_path :
    Context.t ->
    SMT.Term.t ->
    SMT.Term.t ->
    SMT.Term.t ->
    SMT.Term.t ->
    SMT.Term.t ->
    bounds ->
    PredicateBounds.Entry.nested_bounds ->
    SMT.Term.t
  (** [path context selector1 selector2 source target sink bounds] *)

  val typed_reach :
    Context.t ->
    SMT.Term.t ->
    SMT.Term.t ->
    SMT.Term.t ->
    Sort.t ->
    bounds ->
    SMT.Term.t

end
