open Logic_sig

module Make ( ) : sig

  include VARIABLE with module Sort = Sort
                    and type t = Identifier.t * Sort.t
  (** @inline *)

  (* TODO: why is this necessary? *)

  val debug : unit -> string

  val show_debug : t -> string

  val nil : t

  val is_nil : t -> bool

  val is_loc : t -> bool

end
