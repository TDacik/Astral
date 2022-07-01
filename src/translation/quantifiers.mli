(* Quantifier prefix
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module Quantifier : sig

  type range =
    | All
    | Range of SMT.term list

  type t =
    | Forall of SMT.term * range
    | Exists of SMT.term * range

  val mk_exists : SMT.term -> t
  val mk_forall : SMT.term -> t
  val mk_exists_range : SMT.term -> SMT.term list -> t
  val mk_forall_range : SMT.term -> SMT.term list -> t

  val negate : t -> t

  val is_universal : t -> bool

  val is_existential : t -> bool

  val apply : SMT.term -> t -> SMT.term
  (** Apply quantifier to a formula *)

  val show : t -> string

end

(** Sequence of quantifiers computed separately from a formulae *)
module QuantifierPrefix : sig

  type t = Quantifier.t list

  val empty : t

  val add : t -> Quantifier.t -> t

  val prepend : Quantifier.t -> t -> t

  val join : t -> t -> t

  val join_choice : t -> t -> [`Exists | `Forall] -> ?range:SMT.Term.t list option -> SMT.Term.t -> t

  val negate : t -> t

  val drop_implicit : t -> t
  (** Drop implicit existential quantifiers from prefix *)

  val apply : SMT.term -> t -> SMT.term
  (** Apply prefix to a formula *)

  val show : t -> string

end
