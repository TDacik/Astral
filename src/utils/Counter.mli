(* Simple imperative counter.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

module type CONFIG = sig
  val name : string
end

module type CONFIG_MAP = sig
  type t
  val compare : t -> t -> int
  val name : string
end

module type COUNTER := sig

  type t

  val increment : t -> unit
  (** Increase value of the counter. *)

  val decrement : t -> unit
  (** Decrease value of the counter. *)

  val get : t -> int
  (** Get current value of the counter. *)

  val next : t -> int
  (** Increase value of the counter and return it. *)

end

module Simple (_ : CONFIG) : COUNTER with type t := unit

module Map (C : CONFIG_MAP) : COUNTER with type t := C.t
