(* Rich syntax of SSL.
 *
 * This module is intended for pretty-printing of formulae. All computations should
 * be performed with formulas in minimal syntax as defined in module SSL.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Logic_sig

module Variable = SSL.Variable

type t =
  | Var of SSL.Variable.t     (* Location variable *)
  | Pure of SMT.Term.t        (* Pure boolean term which does not contain location variables *)

  (* Atoms *)
  | Eq of t list
  | Distinct of t list
  | PointsTo of t * t list
  | LS of t * t
  | DLS of t * t * t * t
  | SkipList of int * t * t

  (* Boolean connectives *)
  | And of t * t
  | Or of t * t
  | Not of t
  | GuardedNeg of t * t

  (* Quantifiers *)
  | Exists of t list * t
  | Forall of t list * t

  (* Spatial connectives *)
  | Star of t list
  | Septraction of t * t

  (* Syntactic sugar *)
  | True
  | False
  | Emp
  | Implies of t * t
  | Iff of t * t

let rec of_ssl phi = match phi with
  (* Syntactic sugar *)
  | phi when SSL.is_true phi -> True
  | phi when SSL.is_false phi -> False
  | phi when SSL.is_emp phi -> Emp
  | (SSL.Or (SSL.Not lhs, rhs)) -> Implies (of_ssl lhs, of_ssl rhs)
  | (SSL.Or (rhs, SSL.Not lhs)) -> Implies (of_ssl lhs, of_ssl rhs)
  | SSL.And (lhs, rhs) ->
    begin try
      let lhs', rhs' = SSL.get_iff_operands phi in
      Iff (of_ssl lhs', of_ssl rhs')
    with _ -> And (of_ssl lhs, of_ssl rhs)
    end

  (* Other formulae *)
  | SSL.Var v -> Var v
  | SSL.Pure t -> Pure t
  | SSL.Eq xs -> Eq (List.map of_ssl xs)
  | SSL.Distinct xs -> Distinct (List.map of_ssl xs)
  | SSL.PointsTo (x, ys) -> PointsTo (of_ssl x, List.map of_ssl ys)
  | SSL.LS (x, y) -> LS (of_ssl x, of_ssl y)
  | SSL.DLS (x, y, f, l) -> DLS (of_ssl x, of_ssl y, of_ssl f, of_ssl l)
  | SSL.SkipList (depth, x, y) -> SkipList (depth, of_ssl x, of_ssl y)
  | SSL.Or (psi1, psi2) -> Or (of_ssl psi1, of_ssl psi2)
  | SSL.Not psi -> Not (of_ssl psi)
  | SSL.GuardedNeg (psi1, psi2) -> GuardedNeg (of_ssl psi1, of_ssl psi2)
  | SSL.Exists (xs, psi) -> Exists (List.map of_ssl xs, of_ssl psi)
  | SSL.Forall (xs, psi) -> Forall (List.map of_ssl xs, of_ssl psi)
  | SSL.Star psis -> Star (List.map of_ssl psis)
  | SSL.Septraction (psi1, psi2) -> Septraction (of_ssl psi1, of_ssl psi2)

and to_ssl = function
  | Var v -> SSL.Var v
  | Pure t -> SSL.Pure t
  | Eq xs -> SSL.Eq (List.map to_ssl xs)
  | Distinct xs -> SSL.Distinct (List.map to_ssl xs)
  | PointsTo (x, ys) -> SSL.PointsTo (to_ssl x, List.map to_ssl ys)
  | LS (x, y) -> SSL.LS (to_ssl x, to_ssl y)
  | DLS (x, y, f, l) -> SSL.DLS (to_ssl x, to_ssl y, to_ssl f, to_ssl l)
  | SkipList (depth, x, y) -> SSL.SkipList (depth, to_ssl x, to_ssl y)
  | And (psi1, psi2) -> SSL.And (to_ssl psi1, to_ssl psi2)
  | Or (psi1, psi2) -> SSL.Or (to_ssl psi1, to_ssl psi2)
  | Not psi -> SSL.Not (to_ssl psi)
  | GuardedNeg (psi1, psi2) -> SSL.GuardedNeg (to_ssl psi1, to_ssl psi2)
  | Exists (xs, psi) -> SSL.Exists (List.map to_ssl xs, to_ssl psi)
  | Forall (xs, psi) -> SSL.Forall (List.map to_ssl xs, to_ssl psi)
  | Star psis -> SSL.Star (List.map to_ssl psis)
  | Septraction (psi1, psi2) -> SSL.Septraction (to_ssl psi1, to_ssl psi2)

  | True -> SSL.mk_true ()
  | False -> SSL.mk_false ()
  | Emp -> SSL.mk_emp ()
  | Implies (lhs, rhs) -> SSL.mk_implies (to_ssl lhs) (to_ssl rhs)
  | Iff (lhs, rhs) -> SSL.mk_iff [to_ssl lhs; to_ssl rhs]

let description_of_ssl phi = match SSL.describe_node @@ to_ssl phi with
  | name, Logic_sig.Var (x, sort) -> name, Logic_sig.Var (x, sort)
  | name, Operator (xs, sort) -> name, Operator (List.map of_ssl xs, sort)
  | name, Connective xs -> name, Connective (List.map of_ssl xs)
  | name, Quantifier (xs, psi) -> name, Quantifier (List.map of_ssl xs, of_ssl psi)

let describe_node = function
  (* Syntactic sugar *)
  | True -> ("true", Connective [])
  | False -> ("false", Connective [])
  | Emp -> ("emp", Connective [])
  | Implies (lhs, rhs) -> ("=>", Connective [lhs; rhs])
  | Iff (lhs, rhs) -> ("<=>", Connective [lhs; rhs])
  | other -> description_of_ssl other

module Self = struct
  type nonrec t = t
  let describe_node = describe_node
end

include Logic.Make(Self)
