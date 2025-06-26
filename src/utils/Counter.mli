(* Simple imperative counter.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

module Simple () : sig

  val inc : unit -> unit
  (** Increase value of the counter. *)

  val dec : unit -> unit
  (** Decrease value of the counter. *)

  val get : unit -> int
  (** Get current value of the counter. *)

  val next : unit -> int
  (** Increase value of the counter and return it. *)

end
