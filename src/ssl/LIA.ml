(* Experiments with linear integer arithmetic (LIA)
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module Arith = Z3.Arithmetic.Integer

type t =
  | Var of String.t
  | Int of Int.t
  | Bool of Bool.t
  | Plus of t * t
  | Minus of t * t
  | Mult of t * t
  [@@deriving compare, equal]

let rec show = function
  | Var v -> v
  | Bool b -> Format.asprintf "%b" b
  | Int x -> Format.asprintf "%d" x
  | Plus (x, y) -> Format.asprintf "(%s + %s)" (show x) (show y)
  | Minus (x, y) -> Format.asprintf "(%s - %s)" (show x) (show y)
  | Mult (x, y) -> Format.asprintf "(%s * %s)" (show x) (show y)

(** Parse from smtlib format *)
let rec parse expr =
  let open Dolmen.Std.Term in
  match expr.term with
  | App (fn, [x; y]) ->
      let tx = parse x in
      let ty = parse y in
      begin match fn.term with
        | Symbol id -> begin match id.name with
          | "+" -> Plus (tx, ty)
          | "-" -> Minus (tx, ty)
          | "*" -> Mult (tx, ty)
          | s -> failwith ("[LIA parser] Unknown symbol " ^ s)
        end
      end
  | Symbol id ->
    let re = Str.regexp "[0-9]+" in
    if Str.string_match re id.name 0
    then Int (int_of_string id.name)
    else match id.name with
    | "true" -> Bool true
    | "false" -> Bool false
    | _ -> Var id.name

(** Translation to Z3 term *)
let rec translate context term = match term with
  | Var v -> Z3.Boolean.mk_const_s context v
  | Int x -> Arith.mk_numeral_i context x
  | Bool true -> Z3.Boolean.mk_true context
  | Bool false -> Z3.Boolean.mk_false context
  | Plus (x, y) | Minus (x, y) | Mult (x, y) ->
      let tx = translate context x in
      let ty = translate context y in
      match term with
      | Plus _ -> Z3.Arithmetic.mk_add context [tx; ty]
      | Minus _ -> Z3.Arithmetic.mk_sub context [tx; ty]
      | Mult _ -> Z3.Arithmetic.mk_mul context [tx; ty]

module Self = struct
  type nonrec t = t
  let show = show
end

include Datatype.Printable(Self)
