(* Utilities for parsing.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

(*** ==== Exceptions ==== *)

let show_msg = function
  | `Regular msg -> Format.asprintf "%t" msg
  | `Advanced (e, m1, m2, m3) ->
    Format.asprintf "%s\n%t\n%t\n%t\n" e m1 m2 m3

(*** ==== Functions to lift constructors to tuples ==== *)

let incorrect_arity loc name arity =
  let msg = Format.asprintf "Operator %s expects %s operands" name arity in
  ParserException.raise_syntax_error loc msg

let lift_cons ?loc fn operator = function
  | [x1] -> fn x1
  | _ -> incorrect_arity loc operator "exactly 1"

let lift_cons2 ?loc fn operator = function
  | [x1; x2] -> fn x1 x2
  | _ -> incorrect_arity loc operator "exactly 2"

let lift_cons3 ?loc fn operator = function
  | [x1; x2; x3] -> fn x1 x2 x3
  | _ -> incorrect_arity loc operator "exactly 3"

let lift_cons4 ?loc fn operator = function
  | [x1; x2; x3; x4] -> fn x1 x2 x3 x4
  | _ -> incorrect_arity loc operator "exactly 4"

(** Lift n-ary bitvector-operation constructor to non-empty list, where the first element
    is needed to get the width of the resutl. *)
let lift_bitvector_list ?loc name constructor xs = match xs with
  | [] -> incorrect_arity loc name  "at least 1"
  | x :: _ -> constructor (Sort.get_width @@ BaseLogic.get_sort x) xs
