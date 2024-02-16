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

  (** {2 Declarations} *)

  val global_decls : Context.t -> string

  val declare_var : SSL.Variable.t -> string

  val declare_sort : Sort.t -> string

  (** {2 Metadata} *)

  val comment_prefix : string

  val set_status : Context.t -> string

  (** {2 Conversion functions} *)

  val convert_var : SSL.Variable.t -> string

  val convert : SSL.t -> string

  val convert_benchmark : SSL.t -> string

end

module type CONVERTOR = sig

  include CONVERTOR_BASE

  val convert : Context.t -> string

  val dump : string -> Context.t -> unit

end
