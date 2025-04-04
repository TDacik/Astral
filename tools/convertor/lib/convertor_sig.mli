(* Signature of a convertor to other tool format.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

exception UnsupportedFragment of string
(** Exception to signalise unsupported feature. *)

type parameters = {
  name : string;            (** Name of the target tool. *)
  suffix: string;           (** Suffix of input files used by the target tool. *)

  supports_sat : bool;
  (** True if the tool supports satisfiability checking. If not, satisfiability of formula
      phi is translated as phi |= false. *)

  supports_variadic_ops : bool;
  (** True if the tool supports variadic versions of stars, conjunctions, etc. If not, variadic
      operators are replaced by their binary versions.

      TODO: needed? *)

  precise_semantics : bool;
  (** True if the tool uses precise semantics of (dis)equalities (they can be satisfied in empty
      heaps only). *)
}

module type CONVERTOR_BASE = sig

  val params : parameters

  val init : Context.t -> unit

  val comment : string -> string
  (** Generate comment using provided string. *)

  val set_status : [ `Sat | `Unsat | `Unknown] -> string
  (** Generate line setting expected status, may be empty string. *)

  (** {2 Declarations} *)

  val declare_sort : Sort.t -> string

  val declare_var : SL.Variable.t -> string

  val declare_struct : MemoryModel.StructDef.t -> string

  val declare_heap_sort : HeapSort.t -> string

  val declare_predicate : InductiveDefinition.t -> string

  (** {2 Commands} *)

  val add_check_sat : SL.t -> string

end

module type CONVERTOR = sig

  include CONVERTOR_BASE

  val convert : Context.t -> string

  val convert_and_store : Context.t -> string -> unit

end
