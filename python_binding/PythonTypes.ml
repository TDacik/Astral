open Python_lib

module Astral = Astral_lib

(** Duplicated types *)

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
  [@@deriving python]

end

module Variable = struct

  type t = string * Sort.t [@@deriving python]

  type smt_var = string * Sort.t [@@deriving python]

end

module Bitvector = struct

  type t = int * int [@@deriving python]

end

module SMT = struct

  type t = Astral.SMT.t =
    | Constant of string * Sort.t
    | Variable of Variable.smt_var
    | And of t list
    | Or of t list
    | Not of t
    | Implies of t * t
    | Iff of t * t
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
    | Disjoint of t * t
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

  type t = Astral.SSL.t =
    | Var of Variable.t
    | Pure of SMT.t
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
    | Star of t * t
    | Septraction of t * t
  [@@deriving python]

end

(*
module Model = struct

  module Location = Int
  module LocationTupple = struct type t = Location.t list end
  module Stack = Map.Make(Variable)
  module Heap = Map.Make(Location)

  type t = {
    stack : Stack.t;
    heap : Heap.t;
    footprints : Footprint.t SSL.Map.t;
    heaps : Heap.t SSL.Map.t;

    [@@deriving python]

end
*)
