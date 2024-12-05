module type TESTABLE_BASE = sig
  type t
  val equal : t -> t -> bool
  val show : t -> string
end

module type TESTABLE_FULL = sig
  type t
  val check_apply : input:t -> expected:t -> (t -> t) -> unit

  val check_equal : t -> t -> unit
  val check : (t -> bool) -> t -> unit
  val check2 : (t -> t -> bool) -> t -> t -> unit
  val check_list : (t list -> bool) -> t list -> unit
  val check_not : (t -> bool) -> t -> unit
  val check_not2 : (t -> t -> bool) -> t -> t -> unit
  val check_not_list : (t list -> bool) -> t list -> unit
  val check_int : (t -> int) -> t -> int -> unit
end
