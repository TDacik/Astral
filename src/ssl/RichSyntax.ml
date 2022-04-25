(* Rich syntax for SSL
 *
 * This module is intended for pretty-printing of formulae. All computations should
 * be performed with formulas in minimal syntax as defined in module SSL.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module Variable = SSL.Variable

type t =
  | And of t * t
  | Or of t * t
  | Not of t
  | GuardedNeg of t * t
  | Star of t * t
  | Septraction of t * t
  | LS of Variable.t * Variable.t
  | PointsTo of Variable.t * Variable.t
  | Eq of Variable.t * Variable.t
  | Neq of Variable.t * Variable.t

  | True
  | False
  | Emp
  | Wand of t * t
[@@deriving compare, equal]

type arity =
  | Atom'
  | Atom of Variable.t * Variable.t
  | Unary of t
  | Binary of t * t

let get_arity = function
  | And (f1, f2) -> Binary (f1, f2)
  | Or (f1, f2) -> Binary (f1, f2)
  | Not f -> Unary f
  | GuardedNeg (f1, f2) -> Binary (f1, f2)
  | Star (f1, f2) -> Binary (f1, f2)
  | Septraction (f1, f2) -> Binary (f1, f2)
  | LS (v1, v2) -> Atom (v1, v2)
  | PointsTo (v1, v2) -> Atom (v1, v2)
  | Eq (v1, v2) -> Atom (v1, v2)
  | Neq (v1, v2) -> Atom (v1, v2)

  | True -> Atom'
  | False -> Atom'
  | Emp -> Atom'
  | Wand (f1, f2) -> Binary (f1, f2)

let is_emp = function
  | SSL.Eq (Nil, Nil) -> true
  | _ -> false

let is_true = function
  | SSL.Or (SSL.Not psi1, psi2) -> is_emp psi1 && is_emp psi2
  | _ -> false

let is_false = function
  | SSL.And (SSL.Not psi1, psi2) -> is_emp psi1 && is_emp psi2
  | SSL.Not psi -> is_true psi

let rec sugarize phi =
  if is_emp phi then Emp
  else if is_true phi then True
  else if is_false phi then False
  else match phi with
    | SSL.And (f1, f2) -> And (sugarize f1, sugarize f2)
    | SSL.Or (f1, f2) -> Or (sugarize f1, sugarize f2)
    | SSL.Not f -> Not (sugarize f)
    | SSL.GuardedNeg (f1, f2) -> GuardedNeg (sugarize f1, sugarize f2)
    | SSL.Star (f1, f2) -> Star (sugarize f1, sugarize f2)
    | SSL.Septraction (f1, f2) -> Septraction (sugarize f1, sugarize f2)
    | SSL.LS (v1, v2) -> LS (v1, v2)
    | SSL.PointsTo (v1, v2) -> PointsTo (v1, v2)
    | SSL.Eq (v1, v2) -> Eq (v1, v2)
    | SSL.Neq (v1, v2) -> Neq (v1, v2)
