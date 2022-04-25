(* Quantifier prefix
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module Quantifier : sig

  type t =
    | Forall of Z3.Expr.expr
    | Exists of Z3.Expr.expr

  val negate : t -> t

  val is_universal : t -> bool

  val is_existential : t -> bool

  val apply : Context.t -> Z3.Expr.expr -> t -> Z3.Expr.expr
  (** Apply quantifier to a formula *)

  val show : t -> string

end

(** Sequence of quantifiers computed separately from a formulae *)
module QuantifierPrefix : sig

  type t = Quantifier.t list

  val empty : t

  val add : t -> Quantifier.t -> t

  val concat : t -> t -> t

  val negate : t -> t

  val drop_implicit : t -> t
  (** Drop implicit existential quantifiers from prefix *)

  val apply : Context.t -> Z3.Expr.expr -> t -> Z3.Expr.expr
  (** Apply prefix to a formula *)

  val show : t -> string

end
