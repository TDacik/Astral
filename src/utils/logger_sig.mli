(* Signatures for printing
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module type CONFIG = sig

  val name : string

  val level : int

end

module type CONFIG_WITH_DIR = sig
  include CONFIG

  val dirname : string

end

module type CONFIG_WITH_DUMP = sig
  include CONFIG_WITH_DIR

  type t

  val dump : t -> string -> unit

end

(** {2 Output signatures} *)

module type LOGGER = sig

  (*  val info : ('a, Format.formatter, unit) format -> 'a*)

  val warning : ('a, Format.formatter, unit) format -> 'a

  val error : ('a, Format.formatter, unit) format -> 'a

  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a

end

module type LOGGER_WITH_DIR = sig

  include LOGGER

  val dump : (string -> 'a -> unit) -> string -> 'a -> unit

  val dump_string : filename:string -> string -> unit

end

module type LOGGER_WITH_DUMP = sig

  include LOGGER_WITH_DIR

  type t

  val dump : filename:string -> t -> unit

end
