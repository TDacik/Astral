(* Internal representation of first-order formulae.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module BV = Bitvector

module A = BaseLogic.Application
module B = BaseLogic
include B

let to_base_logic = Fun.id

let of_base_logic =
  BaseLogic.map (fun psi -> match psi with
    | Application(GuardedNot, [lhs; rhs]) -> Boolean.mk_and [lhs; Boolean.mk_not rhs]
    | psi -> psi
  )

type var = B.Variable.t

type range = t list Lazy.t list

type view =
  | Constant of String.t * Sort.t
  | Variable of Variable.t

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
  | Exists of Variable.t list * range option * t
  | Forall of Variable.t list * range option * t

  (* Second-order quantifiers *)
  | Exists2 of Variable.t list * range option * t
  | Forall2 of Variable.t list * range option * t

  (* Integer arithmetic *)
  | IntConst of int
  | Plus of t list
  | Minus of t * t
  | Mult of t list

  (* Bitvectors *)
  | BitConst of BV.t
  | BitCheck of t * t
  | BitNot of t
  | BitAnd of t list * Sort.t
  | BitOr of t list * Sort.t
  | BitXor of t list * Sort.t
  | BitPlus of t list * Sort.t
  | BitMult of t list * Sort.t
  | BitImplies of t * t
  | BitNeg of t
  | BitShiftLeft of t * t    (* bitvector, integer *)
  | BitShiftRight of t * t   (* bitvector, integer *)
  | BitLesser of t * t
  | BitLesserEqual of t * t
  | BitExtraction of int * int * t
  | BitExtensionZero of int * t
  | BitExtensionSign of int * t

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

let view phi =
  match phi with
  | B.Variable var -> Variable var
  | B.Application (app, xs) -> begin match app with
    | A.Equal -> Equal xs
    | A.Distinct -> Distinct xs

    | A.Constant (Bool true) -> True
    | A.Constant (Bool false) -> False
    | A.Constant (Int i) -> IntConst i
    | A.Constant (Const (c, sort)) -> Constant (Identifier.show c, sort)
    | A.Constant (Bitvector bv) -> BitConst bv
    | A.And -> And xs
    | A.Or -> Or xs
    | A.Not -> Not (List.hd xs)
    | A.Implies -> Implies (List.nth xs 0, List.nth xs 1)
    | A.Iff -> Iff xs
    | A.IfThenElse -> IfThenElse (List.nth xs 0, List.nth xs 1, List.nth xs 2)


    | A.Plus -> Plus xs
    | A.Minus -> Minus (List.nth xs 0, List.nth xs 1)
    | A.Mult -> Mult xs
    | A.LesserEqual -> LesserEq (List.nth xs 0, List.nth xs 1)

    | A.Membership -> Membership (List.nth xs 0, List.nth xs 1)
    | A.Subset -> Subset (List.nth xs 0, List.nth xs 1)
    | A.Disjoint -> Disjoint xs
    | A.Union sort -> Union (xs, sort)
    | A.Inter sort -> Inter (xs, sort)
    | A.Diff -> Diff (List.nth xs 0, List.nth xs 1)
    | A.Compl -> Compl (List.hd xs)
    | A.Enum sort -> Enumeration (xs, sort)

    | A.BitCheck -> BitCheck (List.nth xs 0, List.nth xs 1)
    | A.BitAnd sort -> BitAnd (xs, Sort.mk_bitvector sort)
    | A.BitOr sort -> BitOr (xs, Sort.mk_bitvector sort)
    | A.BitXor sort -> BitXor (xs, Sort.mk_bitvector sort)
    | A.BitPlus sort -> BitPlus (xs, Sort.mk_bitvector sort)
    | A.BitMult sort -> BitMult (xs, Sort.mk_bitvector sort)
    | A.BitImplies -> BitImplies (List.nth xs 0, List.nth xs 1)
    | A.BitNot -> BitNot (List.nth xs 0)
    | A.BitNeg -> BitNeg (List.nth xs 0)
    | A.BitShiftLeft -> BitShiftLeft (List.nth xs 0, List.nth xs 1)
    | A.BitShiftRight -> BitShiftRight (List.nth xs 0, List.nth xs 1)
    | A.BitUnsignedLesser -> BitLesser (List.nth xs 0, List.nth xs 1)
    | A.BitUnsignedLesserEqual -> BitLesserEqual (List.nth xs 0, List.nth xs 1)
    | A.BitExtraction (left, right) -> BitExtraction (left, right, List.nth xs 0)
    | A.BitExtensionZero width -> BitExtensionZero (width, List.nth xs 0)
    | A.BitExtensionSign width -> BitExtensionSign (width, List.nth xs 0)


    | A.ConstArray sort -> ConstArr (List.nth xs 0, sort)
    | A.Select -> Select (List.nth xs 0, List.nth xs 1)
    | A.Store -> Store (List.nth xs 0, List.nth xs 1, List.nth xs 2)
    | other ->
      failwith @@ Format.asprintf "Internal error: unexpected application %s in SMT term" (A.show other)
  end
  | B.Binder (Exists r, vs, body) -> Exists (vs, r, body)
  | B.Binder (Forall r, vs, body) -> Forall (vs, r, body)
  | B.Binder (Exists2 r, vs, body) -> Exists2 (vs, r, body)
  | B.Binder (Forall2 r, vs, body) -> Forall2 (vs, r, body)

(** TODO: remove duplicate code with SL *)
let rec map_view fn phi =
  let apply phi = match fn @@ view phi with
    | `Modify res -> res
    | `Skip -> phi
  in
  match phi with
  | B.Variable _ -> apply phi
  | B.Application (app, psis) ->
    let args = List.map (map_view fn) psis in
    apply @@ Application (app, args)
  | B.Binder (binder, vs, psi) ->
    let body = map_view fn psi in
    apply @@ Binder (binder, vs, body)

(** TODO: Theory classification

type theory = {
  quantifier_free: bool;
  arithmetic: bool;
  bitvectors: bool;
  arrays: bool;
}

let show_theory {quantifier_free; arithmetic; bitvectors; arrays} =
  let prefix = if quantifier_free then "QF_" else "" in
  let body =
    if arithmetic && not bitvectors then "LIA"
    else if bitvectors && not arithmetic then "BV"
    else "ALL"
  in
  let arrays = if arrays then "A" else "" in
  match body with
    | "ALL" -> "ALL"
    | _ -> prefix ^ arrays ^ body

let classify_theory phi = {
  quantifier_free = is_quantifier_free phi;
  arithmetic = has_sort Sort.int;

*)

(** {2 Theories} *)

include BaseLogic.Equality
module Boolean = BaseLogic.Boolean
module Arithmetic = BaseLogic.Arithmetic
module Enumeration = BaseLogic.Enumeration
module Sets = BaseLogic.Sets
module Array = BaseLogic.Array
module Quantifier = BaseLogic.Quantifiers

module Range = struct

  type t = range

  let map fn = function
    | None -> None
    | Some xss -> Some (List.map (fun xs -> Lazy.map (fun x -> List.map fn x) xs) xss)

end

(** {2 Models} *)

module U = UnicodeSymbols

module Model = struct

  module Term = BaseLogic

  include Variable.MonoMap(Constant)

  let show model =
    bindings model
    |> List.map (fun (v, i) -> Format.asprintf "%s %s %s" (Variable.show v) !U.defined (Constant.show i))
    |> String.concat "\n"

  let show_with_sorts model =
    bindings model
    |> List.map
        (fun (v, i) -> Format.asprintf "%s %s %s" (Variable.show_with_sort v) !U.defined (Constant.show i))
    |> String.concat "\n"

  let rec eval model t = match view t with
    | Variable var ->
      begin
        try find var model
        with Not_found ->
          let _ =Format.printf "Not found %s in:\n%s" (Variable.show_with_sort var) (show_with_sorts model) in
          raise Not_found
      end
    | IntConst i -> Constant.mk_int i
    | Constant (c, sort) -> Constant.mk_const sort c
    | BitConst bv -> Constant.mk_bitvector bv
    | Membership (x, set) ->
      Constant.mk_bool @@ BatList.mem_cmp Constant.compare (eval model x) (Constant.get_elems @@ eval model set)
    | Select (arr, i) ->
      Constant.select (eval model arr) (eval model i)
    | Plus xs ->
      Constant.mk_int @@ List.fold_left (fun acc c -> acc + (Constant.get_int @@ eval model c)) 0 xs

    | IfThenElse (c, t, e) ->
      let c = eval model c in
      if Constant.is_true c then eval model t
      else eval model e

    | Or xs -> Constant.mk_bool @@ List.exists Constant.is_true @@ List.map (eval model) xs
    | Equal [x; y] -> Constant.mk_bool @@ Constant.equal (eval model x) (eval model y)
    | Distinct [x; y] -> Constant.mk_bool @@ not @@ Constant.equal (eval model x) (eval model y)

    | Union (xs, _) ->
      Constant.mk_set @@ List.concat_map Constant.get_elems @@ List.map (eval model) xs
    | Enumeration (xs, _) ->
      Constant.mk_set @@ List.map (eval model) xs

    | _ -> failwith ("TODO: eval other: " ^ Term.show t)

  let check model t = Constant.is_true @@ eval model t

  module Self = struct
    type nonrec t = t
    let show = show
  end

  include Datatype.Printable(Self)

end
