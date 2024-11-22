(* Internal representation of first-order formulae.
 *
 * TODO: simplify interface
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT_sig
open Datatype_sig

type t
(** Abstract type of SMT formulae. *)

(**/**)

val of_base_logic : BaseLogic.t -> t
val to_base_logic : t -> BaseLogic.t

(**/**)

type var

type range = t list Lazy.t list

type view =
  | Constant of String.t * Sort.t
  | Variable of var

  (* Propositional logic *)
  | True
  | False
  | And of t list
  | Or of t list
  | Not of t
  | Implies of t * t
  | Iff of t list
  | IfThenElse of t * t * t

  (* Polymorphic operators *)
  | Equal of t list
  | Distinct of t list
  | LesserEq of t * t

  (* First-order quantifiers *)
  | Exists of var list * range option * t
  | Forall of var list * range option * t

  (* Second-order quantifiers *)
  | Exists2 of var list * range option * t
  | Forall2 of var list * range option * t

  (* Integer arithmetic *)
  | IntConst of int
  | Plus of t list
  | Minus of t * t
  | Mult of t list

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
  | BitLesser of t * t
  | BitLesserEqual of t * t

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

include Logic_sig.LOGIC with
  type t := t
  and module Sort = Sort
  and type Variable.t = var
  and type term := t

include Logic_sig.WITH_VIEW with
  type t := t
  and type view := view

include EQUALITY with type t := t

val is_var : t -> bool

val is_constant : t -> bool

val to_constant : t -> Constant.t

module Boolean : sig

  include EQUALITY with type t := t

  val mk_var : string -> t
  (* Create boolean variable. *)

  val mk_fresh_var : string -> t
  (* Create fresh boolean variable with given prefix. *)

  val mk_const : bool -> t
  val tt : t
  val ff : t

  val mk_and : t list -> t
  val mk_or : t list -> t
  val mk_not : t -> t
  val mk_implies : t -> t -> t
  val mk_iff : t list  -> t
  val mk_ite : t -> t -> t -> t

  val mk_and2 : t -> t -> t
  val mk_or2 : t -> t -> t

  val mk_multiple_ite : (t * t) list -> t -> t

end

module Range : sig

  type term := t

  type t = range

  val map : (term -> term) -> t option -> t option

end

module Quantifier : sig

  val mk_forall : Variable.t list -> ?ranges: Range.t -> t -> t
  val mk_exists : Variable.t list -> ?ranges: Range.t -> t -> t

  val mk_exists' : Sort.t list -> (t list -> t) -> t
  val mk_forall' : Sort.t list -> (t list -> t) -> t

  (*
  val mk_forall_diagonal : t -> t -> t list -> t -> t

  val mk_forall_path : t -> t -> int -> t -> t -> t

  val mk_forall_path_nested2 : t -> t -> t -> (Interval.t * Interval.t list * Interval.t) -> t list -> t -> t


  (* TODO: move elsewhere *)
  let mk_forall_diagonal _ _ _ _ = failwith "Base: forall diagonal"
  let mk_forall_path _ _ _ _ _ = failwith "Base: forall diagonal"
  let mk_forall_path_nested2 _ _ _ _ _ _ = failwith "Base: forall diagonal"
  *)

  val mk_forall2 : Variable.t list -> ?ranges: Range.t -> t -> t
  val mk_exists2 : Variable.t list -> ?ranges: Range.t -> t -> t

  val mk_forall2_range : Variable.t list -> Range.t option -> t -> t
  val mk_exists2_range : Variable.t list -> Range.t option -> t -> t

end

module Enumeration : sig

  include EQUALITY with type t := t

  val mk_var : string -> Sort.t -> t

  val mk_fresh_var : string -> Sort.t -> t

  val mk_sort : string -> string list -> Sort.t

  val mk_const : Sort.t -> string -> t

  val get_constants : Sort.t -> Constant.t list

  val get_constants_terms : Sort.t -> t list

  (*val cardinality : Sort.t -> int*)

end

module Arithmetic : sig

  include EQUALITY with type t := t

  val mk_var : string -> t
  (* Create integer variable. *)

  val mk_fresh_var : string -> t
  (* Create fresh integer variable with given prefix. *)

  val mk_const : int -> t
  (* Create integer constant. *)

  val mk_plus : t list -> t
  val mk_minus : t -> t -> t
  val mk_mult : t list -> t

  val mk_lesser : t -> t -> t
  val mk_lesser_eq : t -> t -> t

end

module Bitvector : sig

  include EQUALITY with type t := t

  val mk_sort : int -> Sort.t
  (* Create bitvector sort with given width. *)

  val mk_var : string -> Sort.t -> t
  val mk_fresh_var : string -> Sort.t -> t

  val mk_const : Bitvector.t -> t
  val mk_const_of_int : int -> int -> t
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

  val mk_plus : int -> t list -> t

  val mk_not : t -> t

  val mk_and : int -> t list -> t
  val mk_or : int -> t list -> t
  val mk_xor : int -> t list -> t

  val mk_implies : t -> t -> t
  val mk_compl : t -> t

  (* TODO: labels? *)
  val mk_shift_left : t -> t -> t
  val mk_shift_right : t -> t -> t

  val mk_lesser : t -> t -> t
  val mk_lesser_eq : t -> t -> t

  val get_width : t -> int

  (** Printing

  val to_bit_string : t -> string

  *)
end

module Array : sig

  include EQUALITY with type t := t

  val mk_var : string -> Sort.t -> t

  val mk_fresh_var : string -> Sort.t -> t

  val mk_sort : Sort.t -> Sort.t -> Sort.t

  val mk_const : t -> Sort.t -> t

  val mk_select : t -> t -> t

  val mk_nary_select : int -> t -> t -> t

  val mk_store : t -> t -> t -> t

end

module Sets : sig

  include EQUALITY with type t := t

  val mk_var : string -> Sort.t -> t

  val mk_fresh_var : string -> Sort.t -> t

  val mk_sort : Sort.t -> Sort.t
  (** Create set sort using given element sort. *)

  val get_elem_sort : t -> Sort.t

  val mk_constant : Sort.t -> t list -> t

  val mk_empty : Sort.t -> t
  val mk_singleton : t -> t
  val mk_constant : Sort.t -> t list -> t

  val mk_add : t -> t -> t
  val mk_union : Sort.t -> t list -> t
  val mk_inter : Sort.t -> t list -> t
  val mk_diff : t -> t -> t
  val mk_compl : t -> t

  val mk_mem : t -> t -> t
  val mk_subset : t -> t -> t
  val mk_disjoint : t list -> t
  val mk_eq_empty : t -> t
  val mk_eq_singleton : t -> t -> t

  val may_disjoint : t list -> bool

end

module Model : sig

  type term := t

  include Datatype_sig.MONO_MAP
    with type key := Variable.t
     and type data := Constant.t
  (** Model is represented as mapping from variables to constants. *)

  include PRINTABLE with type t := t

  val eval : t -> term -> Constant.t
  (** Evaluation of term in model. *)

  val check : t -> term -> bool
  (** Check whether boolean term holds in model. *)

  val show_with_sorts : t -> string

end
