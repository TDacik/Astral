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

  val print : ?prefix:string -> t -> unit
  (** Output to stdout. *)

  val dump : string -> t -> unit
  (** Dump to file given by filename *)

  (** {2 Lists of printable values} *)


  val show_list : ?separator:string -> t list -> string

  val pp_list : Format.formatter -> t list -> unit

  val print_list : ?separator:string -> ?prefix:string -> t list -> unit


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

module type MAP = sig

  type key

  include BatMap.S with type key := key

  val keys : 'a t -> key list

  val values : 'a t -> 'a list

  val find_pred : (key -> bool) -> 'a t -> key

  val show : ('a -> string) -> 'a t -> string

  val show_custom : (key -> string) -> ('a -> string) -> 'a t -> string

end

module type MONO_MAP = sig

  type key
  type data
  type t

  (** Copy-pasted signature of classic polymorphic map *)

  val empty : t

  val add : key -> data -> t -> t

  val mem : key -> t -> bool

  val find : key -> t -> data

  val iter : (key -> data -> unit) -> t -> unit

  val fold : (key -> data -> 'acc -> 'acc) -> t -> 'acc -> 'acc

  val union : (key -> data -> data -> data option) -> t -> t -> t

  val bindings : t -> (key * data) list

  val cardinal : t -> int

  val choose : t -> (key * data)

  val filter : (key -> data -> bool) -> t -> t

  (** Additional functions *)

  val keys : t -> key list

  val values : t -> data list

  val of_list : (key * data) list -> t

  val find_pred : (key -> bool) -> t -> key

  val show : t -> string

  val show_custom : (key -> string) -> (data -> string) -> t -> string

end

module type SET = sig

    type elt

    include BatSet.S with type elt := elt
    include PRINTABLE with type t := t

end

module type COLLECTIONS = sig

  type t

  module Set : SET with type elt = t
  (** Set over type t *)

  module Map : MAP with type key = t
  (** Polymorphic map from t to 'a *)

  module MonoMap (Data : SHOW) : MONO_MAP with type key = t and type data = Data.t
  (** Monomorphic map from t to Data.t *)

end
