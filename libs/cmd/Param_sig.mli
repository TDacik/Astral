(* Signatures of commandline parameters.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

(** Common signature for all commandline parameters. *)
module type PARAM = sig
  val name : string
  val short_name : char option
  val help : string
end

(** Input signature for parameters with value. *)
module type VALUE_IN = sig
  type t
  include PARAM
  val default : t
end

module type VALUE = sig
  include VALUE_IN
  val set : t -> unit
  val get : unit -> t
end

module type INT_IN = sig
  include VALUE_IN with type t := int
  val min : int
  val max : int
end

module type INT = sig
  include VALUE with type t := int
  val min : int
  val max : int
end

module type BOOL = VALUE with type t := bool
module type STRING = VALUE with type t := string

(** Optional *)
module type BOOL_OPTION = VALUE with type t := bool option
module type INT_OPTION = VALUE with type t := int option

module type ENUM_IN = sig
  include PARAM
  type t [@@deriving enum, show]
  val default : t
end

module type ENUM = VALUE

module type ACTION = sig
  include PARAM
  val action : unit -> unit
end
