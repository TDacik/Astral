(* Representation of SSL formulae
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Datatype_sig

(** Simple sorting *)
module Sort : sig

  type t =
    | Loc
    | Int
    | Bool
  [@@deriving compare, equal]

  include PRINTABLE with type t := t

end

(** SSL Variables *)
module Variable : sig

  type var

  type t =
    | Var of var
    | Term of SMT.Term.t
    | Nil

  include PRINTABLE with type t := t
  include COMPARABLE with type t := t
  include COLLECTIONS with type t := t

  val mk : string -> t

  val mk_int : string -> t

  val mk_fresh : string -> t

  val equal : t -> t -> bool

  val hash : t -> int

  val is_nil : t -> bool

end

type formula =
  | And of formula * formula
  | Or of formula * formula
  | Not of formula
  | GuardedNeg of formula * formula
  | Star of formula * formula
  | Septraction of formula * formula
  | LS of Variable.t * Variable.t
  | PointsTo of Variable.t * Variable.t
  | Eq of Variable.t * Variable.t
  | Neq of Variable.t * Variable.t

type t = formula

include PRINTABLE with type t := t
include COMPARABLE with type t := t
include COLLECTIONS with type t := t

type arity =
  | Atom of Variable.t * Variable.t
  | Unary of formula
  | Binary of formula * formula

val get_arity : formula -> arity

val size : formula -> int

val chunk_size : formula -> int

val subformula_id : ?physically:bool -> formula -> formula -> int

(** {2 Fragments} *)

val is_atom : formula -> bool

val is_symbolic_heap : formula -> bool

val is_symbolic_heap_entl : formula -> bool

val is_positive : formula -> bool

type fragment =
  | SymbolicHeap_SAT
  | SymbolicHeap_ENTL
  | Positive
  | Arbitrary

val classify_fragment : formula -> fragment

val normalise : formula -> formula

val mk_eq : Variable.t -> Variable.t -> formula

val mk_neq : Variable.t -> Variable.t -> formula

val mk_distinct : Variable.t list -> formula

val mk_pto : Variable.t -> Variable.t -> formula

val mk_ls : Variable.t -> Variable.t -> formula

val mk_emp : unit -> formula

val mk_true : unit -> formula

val mk_false : unit -> formula

val mk_not : formula -> formula

val mk_star : formula list -> formula

val mk_and : formula list -> formula

val mk_or : formula list -> formula

val mk_septraction : formula -> formula -> formula

val mk_wand : formula -> formula -> formula

(* {2 Fragments} *)

val is_symbolic_heap : formula -> bool
(** Iterated separated conjunction of atoms. *)

val has_unique_footprint : formula -> bool
(** Note: stronger than positive *)

val has_unique_shape : formula -> bool
(** There is at most one h such that (s,h) |= formula *)

val is_list_free : formula -> bool

val fold_on_vars : (Variable.t -> 'a -> 'a) -> 'a -> t -> 'a

val iter_on_subformulas : (t -> unit) -> t -> unit

val fold : (t -> 'a -> 'a) -> t -> 'a -> 'a

val get_vars : ?with_nil:bool -> formula -> Variable.t list
