(* Encoding of bounded reachability.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module Make (E : Translation_sig.ENCODING) : sig

  type bounds := int * int
  (** Type alias for path bounds. *)

  val reach : SMT.t -> SMT.t -> SMT.t -> bounds -> SMT.t
  (** [reach selector source target bounds] *)

  val path : E.Context.t -> SMT.t -> SMT.t -> SMT.t -> bounds -> SMT.t
  (** [path context selector source target bounds] *)

  val path_cons :
    E.Context.t -> SMT.t -> (int -> SMT.t -> SMT.t) -> SMT.t -> SMT.t -> bounds -> SMT.t

  val typed_reach :
    E.Context.t ->
    SMT.t ->
    SMT.t ->
    SMT.t ->
    Sort.t ->
    bounds ->
    SMT.t

end
