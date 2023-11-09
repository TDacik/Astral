(* Internal representation of first-order formulae.
 *
 * TODO: simplify interface
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT_sig
open Logic_sig
open Datatype_sig

module VariableBase : VARIABLE
(** Base module representing variables used in SMT terms. *)

module Range : sig

  type 'a t =
    | Range of 'a list list
    | Pair of ('a * 'a) list
    | Path of ('a * 'a * int) list

  val map : 'a t option -> ('a -> 'b) -> 'b t option
  (** Apply function to all elements of the range.*)

end

type t =
  | Constant of String.t * Sort.t
  | Variable of VariableBase.t

  (* Propositional logic *)
  | And of t list
  | Or of t list
  | Not of t
  | Implies of t * t
  | Iff of t * t
  | IfThenElse of t * t * t
  | True
  | False

  (* Polymorphic operators *)
  | Equal of t list
  | Distinct of t list
  | LesserEq of t * t

  (* First-order quantifiers *)
  | Exists of t list * t Range.t option * t
  | Forall of t list * t Range.t option * t

  (* Second-order quantifiers *)
  | Exists2 of t list * t Range.t option * t
  | Forall2 of t list * t Range.t option * t

  (* Integer arithmetic *)
  | IntConst of int
  | Plus of t * t
  | Minus of t * t
  | Mult of t * t

  (* Bitvectors *)
  | BitConst of Bitvector.t
  | BitCheck of t * t
  | BitAnd of t list * Sort.t
  | BitOr of t list * Sort.t
  | BitXor of t list * Sort.t
  | BitImplies of t * t
  | BitCompl of t
  | BitShiftLeft of t * t    (* bitvector, integer *)
  | BitShiftRight of t * t   (* bitvector, integer *)

  (* Arrays *)
  | ConstArr of t * Sort.t   (* \lambda x : sort. t *)
  | Select of t * t
  | Store of t * t * t

  (* Sets *)
  | Membership of t * t
  | Subset of t * t
  | Disjoint of t list
  | Union of t list * Sort.t
  | Inter of t list * Sort.t
  | Diff of t * t
  | Compl of t
  | Enumeration of t list * Sort.t

  (* Sequences *)
  | Sequence of t list * Sort.t  (* Sequence constant *)
  | SeqIndex of t * t            (* Sequence indexing *)
  | SeqContains of t * t         (* Membership in sequence *)
  | SeqReverse of t              (* Reverse of sequence *)
  [@@deriving fold]

module type SMT_TERM := sig

  include SMT_TERM

  (*val substitute : t -> t -> t -> t
  (** substitute phi x t replaces all free occurrences of x by t in phi. *)
  (** TODO: move to logic *)*)

  val get_sort_in_term : string -> t -> Sort.t
  (** Find sort of variable in term. *)

  val to_smtlib_bench : t -> string

end

module Term : sig

  type nonrec t = t

  include SMT_TERM with type t := t

end

val substitute : t -> t -> t -> t
include SMT_TERM with type t := t

(* ==== Submodules of SMT ==== *)

module Variable : sig

  val of_term : t -> VariableBase.t

  val mk : string -> Sort.t -> t
  (* Create variable of given sort. *)

  val mk_fresh : string -> Sort.t -> t
  (* Create fresh variable with given prefix of given sort. *)

end

module Boolean : sig

  include SMT_TERM with type t := t

  val mk_var : string -> t
  (* Create boolean variable. *)

  val mk_fresh_var : string -> t
  (* Create fresh boolean variable with given prefix. *)

  val mk_true : unit -> t
  val mk_false : unit -> t

  val mk_and : t list -> t
  val mk_or : t list -> t
  val mk_not : t -> t
  val mk_implies : t -> t -> t
  val mk_iff : t -> t -> t
  val mk_ite : t -> t -> t -> t

  val mk_multiple_ite : (t * t) list -> t -> t

end

module Quantifier : sig

  val mk_forall : t list -> ?ranges: t list list option -> t -> t
  val mk_exists : t list -> ?ranges: t list list option -> t -> t

  val mk_forall_diagonal : t -> t -> t list -> t -> t

  val mk_forall_path : t -> t -> int -> t -> t -> t

  val mk_forall_path_nested2 : t -> t -> t -> (Interval.t * Interval.t list * Interval.t) -> t list -> t -> t

  val mk_forall2 : t list -> ?ranges: t list list option -> t -> t
  val mk_exists2 : t list -> ?ranges: t list list option -> t -> t

end

module Enumeration : sig

  include SMT_TERM with type t := t

  val mk_var : string -> Sort.t -> t

  val mk_fresh_var : string -> Sort.t -> t

  val mk_sort : string -> string list -> Sort.t

  val mk_const : Sort.t -> string -> t

  val get_constants : Sort.t -> t list

  val cardinality : Sort.t -> int

end

module Arithmetic : sig

  include SMT_TERM with type t := t

  val mk_var : string -> t
  (* Create integer variable. *)

  val mk_fresh_var : string -> t
  (* Create fresh integer variable with given prefix. *)

  val mk_const : int -> t
  (* Create integer constant. *)

  val mk_plus : t -> t -> t
  val mk_minus : t -> t -> t
  val mk_mult : t -> t -> t

  val mk_lesser_eq : t -> t -> t

end

module Bitvector : sig

  include SMT_TERM with type t := t

  val mk_sort : int -> Sort.t
  (* Create bitvector sort with given width. *)

  val mk_var : string -> Sort.t -> t
  val mk_fresh_var : string -> Sort.t -> t

  val mk_const : int -> int -> t
  val mk_const_of_string : string -> t

  val mk_zero : int -> t
  (* Create bitvector representing zero of given width. *)

  val mk_one : int -> t
  (* Create bitvector representing one of given width. *)

  val mk_full_zeros : int -> t
  (* Create bitvector of 0s of given width. *)

  val mk_full_ones: int -> t
  (* Create bitvector of 1s of given width. *)

  val mk_bit_check : t -> t -> t

  val mk_and : t list -> Sort.t -> t
  val mk_or : t list -> Sort.t -> t
  val mk_xor : t list -> Sort.t -> t

  val mk_implies : t -> t -> t
  val mk_compl : t -> t

  (* TODO: labels? *)
  val mk_shift_left : t -> t -> t
  val mk_shift_right : t -> t -> t

  val mk_lesser_eq : t -> t -> t

  val get_width : t -> int

  (** Printing *)

  val to_bit_string : t -> string

end

module Array : sig

  include SMT_TERM with type t := t

  val mk_var : string -> Sort.t -> t

  val mk_fresh_var : string -> Sort.t -> t

  val mk_sort : Sort.t -> Sort.t -> Sort.t

  val mk_const : t -> Sort.t -> t

  val mk_select : t -> t -> t

  val mk_store : t -> t -> t -> t

end

module Set : sig

  include SMT_TERM with type t := t

  val mk_var : string -> Sort.t -> t

  val mk_fresh_var : string -> Sort.t -> t

  val mk_sort : Sort.t -> Sort.t
  (** Create set sort using given element sort. *)

  val get_elem_sort : t -> Sort.t

  val mk_empty : Sort.t -> t
  val mk_singleton : t -> t
  val mk_enumeration : Sort.t -> t list -> t

  val mk_add : t -> t -> t
  val mk_union : t list -> Sort.t -> t
  val mk_inter : t list -> Sort.t -> t
  val mk_diff : t -> t -> t
  val mk_compl : t -> t

  val mk_mem : t -> t -> t
  val mk_subset : t -> t -> t
  val mk_disjoint : t -> t -> t
  val mk_disjoint_list : t list -> t
  val mk_eq_empty : t -> t
  val mk_eq_singleton : t -> t -> t

  val may_disjoint : t -> t -> bool
  val get_elems : t -> t list

end

(* Pretty printers *)

  val pretty_select : (t -> string) -> t -> string

module Model : sig

  include Map.S with type key := VariableBase.t
  (** Model is represented as mapping from variables to terms. *)

  type model = Term.t t
  (* TODO: it is somehow possible to name this type `t`? *)

  include PRINTABLE with type t := model

  val eval : model -> Term.t -> Term.t
  (** Evaluation of term in model. *)

  val show_with_sorts : model -> string

end
