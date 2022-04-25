(* Signatures for datatypes
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

module type SHOWABLE = sig

  type t

  val show : t -> string
  (** String representation of type t *)

end

module type PRINTABLE = sig

  include SHOWABLE

  val pp : Format.formatter -> t -> unit
  (** Output to formatter *)

end

module type COMPARABLE = sig

  type t

  val compare : t -> t -> int

end

module type COLLECTIONS = sig

  type t

  module Set : Set.S with type elt = t
  (** Set over type t *)

  module Map : Map.S with type key = t
  (** Map from t to 'a *)

end
