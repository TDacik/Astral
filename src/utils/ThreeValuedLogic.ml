(* Boolean with 'unknown' value and utility functions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

type t = True | False | Unknown [@@deving compare, equal]

let of_bool b = if b then True else False

let to_bool default = function
  | True -> true
  | False -> false
  | Unknown -> default

let is_true = (=) True

let is_false = (=) False

let can_be_true = function True | Unknown -> true | False -> false

let not3 = function
  | True -> False
  | False -> True
  | Unknown -> Unknown

let and3 lhs rhs = match lhs, rhs with
  | False, _ | _, False -> False
  | True, True -> True
  | _, _ -> Unknown

let or3 lhs rhs = match lhs, rhs with
  | True, _ | _, True -> True
  | False, False -> False
  | _, _ -> Unknown

let exists (f : 'a -> t) (xs : 'a list) : t =
  List.fold_left (fun acc x -> or3 acc @@ f x) False xs
