(* Utilities for parsing.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

(*** ==== Exceptions ==== *)

type loc = Dolmen_std.Loc.t

exception NotSupported of loc * string

exception SortError of loc * string * string * string (* Kind, actual, expected *)

exception NotDeclared of loc * string * string (* Kind, name *)

exception Redeclared of loc * string * string (* Kind, name *)


exception ParserError of string

exception SortRedefined of Sort.t

exception SortNotDeclared of string

exception VariableRedefined of string

exception VariableNotDeclared of string

exception StructNotDeclared of string

exception ConstructorNotDeclared of string

exception VariableTypeError of SL.Variable.t * Sort.t * Sort.t

let parser_error ?loc ?hint msg =
  Format.fprintf Format.err_formatter "%sParser error:%s %s\n"
    Colors.red Colors.white msg;
  begin match hint with
    | None -> ()
    | Some text -> Format.fprintf Format.err_formatter "\nHint: %s\n" text;
  end;
  exit 2

let show_msg = function
  | `Regular msg -> Format.asprintf "%t" msg
  | `Advanced (e, m1, m2, m3) ->
    Format.asprintf "%s\n%t\n%t\n%t\n" e m1 m2 m3

(*** ==== Functions to lift constructors to tuples ==== *)

let incorrect_arity name arity =
  let msg = Format.asprintf "Operator %s expects %s operands" name arity in
  parser_error msg

let lift_cons fn operator = function
  | [x1] -> fn x1
  | _ -> incorrect_arity operator "exactly 1"

let lift_cons2 fn operator = function
  | [x1; x2] -> fn x1 x2
  | _ -> incorrect_arity operator "exactly 2"

let lift_cons3 fn operator = function
  | [x1; x2; x3] -> fn x1 x2 x3
  | _ -> incorrect_arity operator "exactly 3"

let lift_cons4 fn operator = function
  | [x1; x2; x3; x4] -> fn x1 x2 x3 x4
  | _ -> incorrect_arity operator "exactly 4"

(** Lift n-ary bitvector-operation constructor to non-empty list, where the first element
    is needed to get the width of the resutl. *)
let lift_bitvector_list name constructor xs = match xs with
  | [] -> incorrect_arity name  "at least 1"
  | x :: _ -> constructor (Sort.get_width @@ BaseLogic.get_sort x) xs
