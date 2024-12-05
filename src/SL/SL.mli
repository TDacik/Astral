(* Representation of SL formulae.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2021 *)

open Logic_sig
open Datatype_sig
open MemoryModel

type t

(**/**)

val of_base_logic : BaseLogic.t -> t
val to_base_logic : t -> BaseLogic.t

(**/**)

module Variable : sig
  include VARIABLE with module Sort = Sort

  val nil : t

  val is_nil : t -> bool

  val is_loc : t -> bool

  val is_pure : t -> bool

end

module Term : sig

  type t

  val nil : t

  val is_nil : t -> bool

  val is_heap_term : t -> bool

  val is_smt_term : t -> bool

  val mk_var : string -> Sort.t -> t

  val mk_heap_term : Field.t -> t -> t

  val mk_smt : SMT.t -> t

  val mk_block_begin : t -> t

  val mk_block_end : t -> t

  val of_var : Variable.t -> t

  val get_subterm : t -> t
  (** Assumes that the term is not a variable. *)

  (** TODO: logic_sig? *)

  include Logic_sig.SORTED with type t := t and module Sort = Sort

  include Datatype_sig.COMPARABLE with type t := t
  include Datatype_sig.PRINTABLE with type t := t
  include Datatype_sig.COLLECTIONS with type t := t

  val hash : t -> int

  type view =
    | Var of Variable.t
    | HeapTerm of Field.t * t
    | SmtTerm of SMT.t
    | BlockBegin of t
    | BlockEnd of t

  val view : t -> view

end

(**/**)

(** Some unsafe casting functions *)

val to_smt : t -> SMT.t
val of_term : Term.t -> t
val to_term : t -> Term.t

(**/**)


include LOGIC
  with type t := t
   and type term := Term.t
   and module Sort = Sort
   and module Variable := Variable

type view =
  | Emp
  | True
  | False
  | Pure of SMT.t
  | Eq of Term.t list
  | Distinct of Term.t list
  | PointsTo of Term.t * StructDef.t * Term.t list
  | Predicate of string * Term.t list * StructDef.t list
  | And of t list
  | Or of t list
  | Not of t
  | GuardedNeg of t * t
  | Exists of Variable.t list * t
  | Forall of Variable.t list * t
  | Star of t list
  | Septraction of t * t

include Logic_sig.WITH_VIEW
  with type t := t
   and type view := view


include COLLECTIONS with type t := t

val of_smt : SMT.t -> t

(** {2 Constructors} *)

val nil : t

val mk_var : string -> Sort.t -> t
(** Create SL term consisting of a location variable with given name. *)

val mk_fresh_var : string -> Sort.t -> t
(** Create SL term consisting of a fresh location variable with given name. *)

val mk_eq : Term.t list -> t

val mk_eq2 : Term.t -> Term.t -> t

val mk_distinct : Term.t list -> t

val mk_distinct2 : Term.t -> Term.t -> t

val mk_pto_struct : Term.t -> StructDef.t -> Term.t list -> t
(** Low-level pointer constructor *)

val mk_pto : Term.t -> Term.t -> t
(** Create a term representing a pointer with a single target location. *)

val mk_predicate : string -> ?structs:StructDef.t list -> Term.t list -> t
(** Create an instance of an inductive predicate with given name. This instance can be
    parametrised by a list of structures. For example, Astral's built-in list-segment
    predicate can be parametrised by any structure with a single field with sort same
    as the sort of instantance's arguments.*)

val emp : t

val tt : t

val ff : t

val mk_pure : SMT.t -> t
(** Create a pure assertion as a separation logic formula. The input has to be of boolean
    sort.

    Note that pure formula are satisfiable only on empty footprints. Thus, (pure true) is
    equivalent to emp. *)

val mk_not : t -> t

val mk_star : t list -> t

val mk_and : t list -> t

val mk_or : t list -> t

val mk_implies : t -> t -> t

val mk_iff : t list -> t

val mk_ite : t -> t -> t -> t

val mk_gneg : t -> t -> t

val mk_septraction : t -> t -> t

val mk_wand : t -> t -> t

val mk_exists : Variable.t list -> t -> t

val mk_forall : Variable.t list -> t -> t

val mk_exists' : Sort.t list -> (Term.t list -> t) -> t

val mk_forall' : Sort.t list -> (Term.t list -> t) -> t

(** {2 Properties} *)

val is_pointer : t -> bool

val is_spatial_atom : t -> bool

(** {2 Fragment classification} *)

val is_pure : t -> bool
(** Boolean combination of (dis-)equalities over location variables. *)

val is_pure_smt : t -> bool
(** No location variables. *)

val is_atom : t -> bool

val is_nil : t -> bool

val is_predicate : t -> bool

val is_atomic : t -> bool

val is_symbolic_heap : t -> bool

val is_symbolic_heap_entl : t -> bool

val is_negation_free : t -> bool

val is_positive : t -> bool

val is_quantifier_free : t -> bool

val is_low_level : t -> bool

type fragment =
  | SymbolicHeap_SAT
  | SymbolicHeap_ENTL
  | Positive
  | Arbitrary
  | Atomic

val show_fragment : fragment -> string

val classify_fragment : t -> fragment

type query =
  | SymbolicHeap_SAT of t list
  | SymbolicHeap_ENTL of t list * t list
  | Arbitrary of t

val as_query : t -> query

val as_entailment : t -> t * t

val get_root : t -> Variable.t

val get_fields : t -> Field.t list

(** {2 Syntactic manipulation} *)

val free_vars : ?with_nil:bool -> ?with_pure:bool -> t -> Variable.t list

val free_vars_of_sort : Sort.t -> t -> Variable.t list

val get_terms : t -> Term.t list

val get_terms_of_sort : Sort.t -> t -> Term.t list

val get_root : t -> Term.t

(** {2 Operations requiring the sort of heap *)

val get_loc_terms : t -> HeapSort.t -> Term.t list

module Infix : sig

  val (==) : Term.t -> Term.t -> t
  (** Infix equality *)

  val (!=) : Term.t -> Term.t -> t
  (** Infix disequality *)

  val (|->) : Term.t -> Term.t -> t
  (** Infix pointer *)

  val (|~>) : Term.t -> Term.t -> t
  (** Infix singly-linked list *)

  val (=>) : t -> t -> t
  (** Infix implication *)

  val (<=>) : t -> t -> t
  (** Infix iff *)

  val (&!) : t -> t -> t
  (** Infix guarded negation *)

  val (&&) : t -> t -> t
  (** Infix conjucntion *)

  val (||) : t -> t -> t
  (** Infix disjunction *)

  val ( * ) : t -> t -> t
  (** Infix separating conjunction *)

end

(** {2 Misc} *)

val as_symbolic_heap : t -> t list * t list
