(* Conversion of Ocaml and Python types
 *
 * To avoid dependencies, all relevant types are duplicated here. Since those types are not identic  * to their counterparts, unsafe casts using Obj.magic are necessary.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Python_lib

exception Not_found_s = Base.Not_found_s

(* === Sorts === *)

type sort =
  | Loc
  | Int
  | Bool
  [@@deriving python]

let sort_of_python x = Caml.Obj.magic @@ sort_of_python x
let python_of_sort x = Caml.Obj.magic @@ python_of_sort x

(* === Variables === *)

type var = {
  id : int;
  sort : sort;
  name : string;
} [@@deriving python]

let var_of_python x = Caml.Obj.magic @@ var_of_python x
let python_of_var x = python_of_var @@ Caml.Obj.magic x

type variable =
  | Var of var
  | Nil
[@@deriving python]

let variable_of_python x = Caml.Obj.magic @@ variable_of_python x
let python_of_variable x = python_of_variable @@ Caml.Obj.magic x

(* === Formulae === *)

type formula =
  | And of formula * formula
  | Or of formula * formula
  | Not of formula
  | GuardedNeg of formula * formula
  | Star of formula * formula
  | Septraction of formula * formula
  | LS of variable * variable
  | PointsTo of variable * variable
  | Eq of variable * variable
  | Neq of variable * variable
[@@deriving python]

let formula_of_python x = Caml.Obj.magic @@ formula_of_python x
let python_of_formula x = python_of_formula @@ Caml.Obj.magic x

type formula_info = {
  formula : formula;
  variables : variable list;
  stack_bound : int * int;
  heap_bound : int;
  must_allocated_locations : int;
} [@@deriving python]

let formula_info_of_python x = Caml.Obj.magic @@ formula_info_of_python x
let python_of_formula_info x = python_of_formula_info @@ Caml.Obj.magic x

(* === Models === *)

type stack = (variable * int) list [@@deriving python]
type heap = (int * int) list [@@deriving python]

type model = stack * heap [@@deriving python]

let model_of_python x = Caml.Obj.magic @@ model_of_python x
let python_of_model x = python_of_model @@ Caml.Obj.magic x
