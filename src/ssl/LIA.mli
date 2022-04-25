(* Experiments with linear integer arithmetic (LIA)
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Datatype_sig

type t =
  | Var of String.t
  | Int of Int.t
  | Bool of Bool.t
  | Plus of t * t
  | Minus of t * t
  | Mult of t * t
  [@@deriving compare, equal]

include PRINTABLE with type t := t

val parse : Dolmen.Std.Term.t -> t

val translate : Z3.context -> t -> Z3.Expr.expr
