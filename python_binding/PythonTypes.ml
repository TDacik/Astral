(* Duplication of core Astral's types.
 *
 * This module duplicates python types in order to avoid python_lib dependencies in
 * the solver. *)

open Base
open Python_lib
open Python_lib.Defunc

module Astral = Astral_lib


module Sort = struct

  type t = Astral.Sort.t =
    | Bool
    | Int
    | Finite of string * string list
    | Set of t
    | Sequence of t
    | Array of t * t
    | Bitvector of int
    | Loc
    | Tupple of t list
    | Sum of t list
    | Uninterpreted of string
  [@@deriving compare, python]

end

module Bitvector = struct

  type t = int * int [@@deriving python]

end

module SMT = struct

  module Variable = struct

    type t = string * Sort.t [@@deriving python]

  end

  type t = Astral.SMT.t =
    | Constant of string * Sort.t
    | Variable of Variable.t
    | And of t list
    | Or of t list
    | Not of t
    | Implies of t * t
    | Iff of t * t
    | IfThenElse of t * t * t
    | True
    | False
    | Equal of t list
    | Distinct of t list
    | LesserEq of t * t
    | Exists of t list * t
    | Forall of t list * t
    | Exists2 of t list * t list list option * t
    | Forall2 of t list * t list list option * t
    | IntConst of int
    | Plus of t * t
    | Minus of t * t
    | Mult of t * t
    | BitConst of Bitvector.t
    | BitCheck of t * t
    | BitAnd of t list * Sort.t
    | BitOr of t list * Sort.t
    | BitXor of t list * Sort.t
    | BitImplies of t * t
    | BitCompl of t
    | BitShiftLeft of t * t
    | BitShiftRight of t * t
    | ConstArr of t * Sort.t
    | Select of t * t
    | Store of t * t * t
    | Membership of t * t
    | Subset of t * t
    | Disjoint of t list
    | Union of t list * Sort.t
    | Inter of t list * Sort.t
    | Diff of t * t
    | Compl of t
    | Enumeration of t list * Sort.t
    | Sequence of t list * Sort.t
    | SeqIndex of t * t
    | SeqContains of t * t
    | SeqReverse of t
  [@@deriving python]

end

module SSL = struct

  module Variable = struct

    type t = string * Sort.t [@@deriving compare, python]

    let _type = Of_python.create ~type_name:"var" ~conv:t_of_python
    let _list = Of_python.create ~type_name:"var list" ~conv:(list_of_python t_of_python)

  end

  type t = Astral.SSL.t =
    | Var of Variable.t
    | Pure of SMT.t
    | Emp
    | Eq of t list
    | Distinct of t list
    | PointsTo of t * t list
    | LS of t * t
    | DLS of t * t * t * t
    | NLS of t * t * t
    | SkipList of int * t * t
    | And of t * t
    | Or of t * t
    | Not of t
    | GuardedNeg of t * t
    | Exists of t list * t
    | Forall of t list * t
    | Star of t list
    | Septraction of t * t
  [@@deriving python]

  let _type = Of_python.create ~type_name:"formula" ~conv:t_of_python
  let _list = Of_python.create ~type_name:"formula list" ~conv:(list_of_python t_of_python)

end

(* TODO: can we derive python of Map.t? *)
module Model = struct

  type stack = (SSL.Variable.t * int) list [@@deriving python]

  type heap = (int * int list) list [@@deriving python]

  type t = stack * heap [@@deriving python]

end
