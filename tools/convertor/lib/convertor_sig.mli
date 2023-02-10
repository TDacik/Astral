(* Signature of a convertor to other tool format.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module type CONVERTOR_BASE = sig

  val name : string
  (** Name of the target tool. *)

  val suffix : string
  (** Suffix of input files used by the target tool. *)


  (** {2 Solver's parameters} *)

  val supports_sat : bool
  (** True if the tool supports satisfiability checking. If not, satisfiability of formula
      phi is translated as phi |= false. *)

  val supports_variadic_operators : bool
  (** True if the tool supports variadic versions of stars, conjunctions, etc. If not, variadic
      operators are replaced by their binary versions. *)

  val precise_semantics : bool
  (** True if the tool uses precise semantics of (dis)equalities (they can be satisfied in empty
      heaps only). *)

  val supports_ls : bool
  (** True if the tool supports singly-linked lists. *)

  val supports_dls : bool
  (** True if the tool supports doubly-linked lists. *)


  (** {2 Conversion functions} *)

  val comment_prefix : string

  val set_status : Context.t -> string

  val declare_var : SSL.Variable.t -> string

  val declare_ls : string

  val declare_dls : string

  val convert_var : SSL.Variable.t -> string

  val convert : SSL.t -> string

  val convert_assert : SSL.t -> string

  val command : string

end

module type CONVERTOR = sig

  include CONVERTOR_BASE

  val convert : Context.t -> string

  val dump : string -> Context.t -> unit

end
