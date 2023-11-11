(* Signatures for datatypes
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

module type SHOW = sig

  type t

  val show : t -> string
  (** String representation of type t *)

end

module type PRINTABLE = sig

  include SHOW

  val pp : Format.formatter -> t -> unit
  (** Output to formatter *)

  val print : t -> unit
  (** Output to stdout. *)

  val dump : string -> t -> unit
  (** Dump to file given by filename *)

end

module type COMPARISON = sig

  type t

  val show : t -> string

  val compare : t -> t -> int

end

module type COMPARABLE = sig

  include COMPARISON

  val equal : t -> t -> bool

end

module type COLLECTIONS = sig

  type t

  module Set : sig

    include BatSet.S with type elt = t

    val show : t -> string

  end
  (** Set over type t *)

  module Map : sig

    include BatMap.S with type key = t

    val keys : 'a t -> key list

    val values : 'a t -> 'a list

    val find_pred : (key -> bool) -> 'a t -> key

    val show : ('a -> string) -> 'a t -> string

  end
  (** Map with structural equality from t to 'a *)

end

module type MONO_MAP = sig

  type t
  type key
  type data

  val empty : t

  val add : key -> data -> t -> t

  val find : key -> t -> data

  val bindings : t -> (key * data) list

end
