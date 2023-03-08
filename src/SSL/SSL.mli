(* Representation of SSL formulae
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Logic_sig
open Datatype_sig

module Variable : sig
  include VARIABLE

  val mk : string -> t

  val mk_sort : string -> Sort.t -> t

  val mk_fresh : string -> t

  val nil : t

  val is_nil : t -> bool

  val hash : t -> int

  include COLLECTIONS with type t := t

end

type t =
  | Var of Variable.t
  | Pure of SMT.Term.t
  | Eq of t list
  | Distinct of t list
  | PointsTo of t * t list
  | LS of t * t
  | DLS of t * t * t * t
  | SkipList of int * t * t
  | And of t * t
  | Or of t * t
  | Not of t
  | GuardedNeg of t * t
  | Exists of t list * t
  | Forall of t list * t
  | Star of t list
  | Septraction of t * t

include LOGIC with type t := t

include COLLECTIONS with type t := t

val hash : t -> int

val (===) : t -> t -> bool
(** More precise implementation of equality that takes into account reordering of sub-formulae.
    The function is intended for unit tests. *)

val chunk_size : t -> int

(** {2 Subformulae ID} *)

val subformula_id : ?physically:bool -> t -> t -> int

val find_by_id : t -> int -> t

(** {2 Fragments} *)

val is_pure : t -> bool

val is_pure_smt : t -> bool

val is_atom : t -> bool

val is_atomic : t -> bool

val is_symbolic_heap : t -> bool

val is_symbolic_heap_entl : t -> bool

val is_negation_free : t -> bool

val is_positive : t -> bool

val is_quantifier_free : t -> bool

type fragment =
  | SymbolicHeap_SAT
  | SymbolicHeap_ENTL
  | Atomic
  | Positive
  | Arbitrary

val classify_fragment : t -> fragment

val normalise : t -> t

val mk_nil : unit -> t

val mk_var : string -> t
(** Create SSL term consisting of a location variable with given name. *)

val mk_fresh_var : string -> t
(** Create SSL term consisting of a fresh location variable with given name. *)

val mk_eq : t -> t -> t
(** Create equality of two terms *)

val mk_eq_list : t list -> t
(** Create equality of list of terms. *)

val mk_distinct : t -> t -> t

val mk_distinct_list : t list -> t

val mk_pto : t -> t -> t
(** Create a term representing a pointer with a single target location. *)

val mk_pto_seq : t -> t list -> t
(** Create a term representing a pointer with a sequence of target locations. *)

val mk_ls : t -> t -> t

val mk_dls : t -> t -> t -> t -> t

val mk_skl : int -> t -> t -> t

val mk_emp : unit -> t

val mk_true : unit -> t

val mk_false : unit -> t

val mk_not : t -> t

val mk_star : t list -> t

val mk_and : t list -> t

val mk_or : t list -> t

val mk_implies : t -> t -> t

val mk_iff : t list -> t

val mk_gneg : t -> t -> t

val mk_septraction : t -> t -> t

val mk_wand : t -> t -> t

val mk_exists : t list -> t -> t

val mk_forall : t list -> t -> t

(* {2 Predicates} *)

val is_true : t -> bool

val is_false : t -> bool

val is_emp : t -> bool

val is_implies : t -> bool

val is_iff : t -> bool

val get_implies_operands : t -> t * t

val get_iff_operands : t -> t * t

(* {2 Fragments} *)

val is_symbolic_heap : t -> bool
(** Iterated separated conjunction of atoms. *)

val has_unique_footprint : t -> bool
(** Note: stronger than positive *)

val has_unique_shape : t -> bool
(** There is at most one h such that (s,h) |= formula *)

val fold_on_vars : (Variable.t -> 'a -> 'a) -> 'a -> t -> 'a

val iter_on_subformulas : (t -> unit) -> t -> unit

val fold : (t -> 'a -> 'a) -> t -> 'a -> 'a

val get_vars : ?with_nil:bool -> t -> Variable.t list

val map_vars : (Variable.t -> t) -> t -> t
(** Apply function to all free variables in formula. *)

val select_subformulae : (t -> bool) -> t -> t list
(** Return all subformulae satisfying given predicate. *)

val substitute_pure : t -> SMT.t -> SMT.t -> t

val substitute : t -> t -> t -> t

module Var : sig

  val is_nil : t -> bool

end

val mk_pure : SMT.Term.t -> t

val mk_pure_var : string -> Sort.t -> t

module Infix : sig

  val (==) : t -> t -> t
  (** Infix equality *)

  val (!=) : t -> t -> t
  (** Infix disequality *)

  val (|->) : t -> t -> t
  (** Infix pointer *)

  val (|~>) : t -> t -> t
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
