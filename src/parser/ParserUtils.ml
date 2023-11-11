(* Utilities for parsing.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)


(*** ==== Exceptions ==== *)

exception ParserError of string

exception SortRedefined of Sort.t

exception SortNotDeclared of Sort.t

exception VariableRedefined of string

exception VariableNotDeclared of string

exception VariableTypeError of SSL.Variable.t * Sort.t * Sort.t


(*** ==== Functions to lift constructors to tuples ==== *)

let incorrect_arity name n =
  let msg = Format.asprintf "Operator %s expects %d operands" name n in
  raise @@ ParserError msg

let lift_cons fn operator = function
  | [x1] -> fn x1
  | _ -> incorrect_arity operator 1

let lift_cons2 fn operator = function
  | [x1; x2] -> fn x1 x2
  | _ -> incorrect_arity operator 2

let lift_cons3 fn operator = function
  | [x1; x2; x3] -> fn x1 x2 x3
  | _ -> incorrect_arity operator 3

let lift_cons4 fn operator = function
  | [x1; x2; x3; x4] -> fn x1 x2 x3 x4
  | _ -> incorrect_arity operator 4
