(* Quantifier prefix
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module Quantifier : sig

  type range = SMT.term list

  type t =
    | Forall of SMT.term * range
    | Exists of SMT.term * range

  val negate : t -> t

  val is_universal : t -> bool

  val is_existential : t -> bool

  val apply : Context.t -> SMT.term -> t -> SMT.term
  (** Apply quantifier to a formula *)

  val show : t -> string

end

(** Sequence of quantifiers computed separately from a formulae *)
module QuantifierPrefix : sig

  type t = Quantifier.t list

  val empty : t

  val add : t -> Quantifier.t -> t

  val join : t -> t -> t

  val join_choice : t -> t -> [`Exists | `Forall] -> SMT.term -> Quantifier.range -> t

  val negate : t -> t

  val drop_implicit : t -> t
  (** Drop implicit existential quantifiers from prefix *)

  val apply : Context.t -> SMT.term -> t -> SMT.term
  (** Apply prefix to a formula *)

  val show : t -> string

end
