(* Signature for set encoding
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

module type MINIMAL_SET = sig

  val mk_const_s : Z3.context -> string -> Z3.Sort.sort -> Z3.Expr.expr

  val mk_fresh_const : Z3.context -> string -> Z3.Sort.sort -> Z3.Expr.expr

  val mk_sort : Z3.context -> Z3.Sort.sort -> Z3.Sort.sort
  (** For sort A return sort A set *)

  val get_elem_sort : Z3.Expr.expr -> Z3.Sort.sort
  (** For set expression return sort of its elements *)

  val mk_empty : Z3.context -> Z3.Sort.sort -> Z3.Expr.expr
  (** Return empty set expression *)

  val mk_universal : Z3.context -> Z3.Sort.sort -> Z3.Expr.expr
  (** Return universal set expression *)

  val mk_eq : Z3.context -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  (** Given sets and A and B, return A = B. Depending on implementation of set, this
      function may produce different results than Booolean.mk_eq. *)

  val mk_mem : Z3.context -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  (** Given set X of sort A set and element x of sort A, return x \in X. *)

  val mk_singleton : Z3.context -> Z3.Expr.expr -> Z3.Expr.expr
  (** Given element x of sort A return expression {x} of sort A set. *)

  val mk_enumeration : Z3.context -> Z3.Sort.sort -> Z3.Expr.expr list -> Z3.Expr.expr
  (** Given sort A and list of expressions [x_1, ..., x_n] of sorts A, return set
      expression {x_1, ..., x_n} of sort A set. *)

  val mk_subset : Z3.context -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  (** Given sets and A and B, return A \subset B. *)

  val mk_union : Z3.context -> Z3.Expr.expr list -> Z3.Expr.expr
  (** Given sets and A and B, return A \union B. *)

  val mk_inter : Z3.context -> Z3.Expr.expr list -> Z3.Expr.expr
  (** Given sets and A and B, return A \inter B. *)

  val mk_difference : Z3.context -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  (** Given sets and A and B, return A \setminus B. *)

  val inverse_translation: Z3.context -> Z3.Model.model -> Z3.Expr.expr -> Z3.Expr.expr list
  (** Get interpretation of a set symbol as a list. *)

end

module type SET = sig

  include MINIMAL_SET

  val mk_distinct : Z3.context -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  (** Given sets A and B, return A != B. *)

  val mk_eq_empty : Z3.context -> Z3.Expr.expr -> Z3.Expr.expr
  (** Given set X, return X = {}. *)

  val mk_eq_universal : Z3.context -> Z3.Expr.expr -> Z3.Expr.expr
  (** Given set X, return X = UNIV. *)

  val mk_eq_singleton : Z3.context -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  (** Given set X and element x, return X = {x}. *)

  val mk_are_disjoint : Z3.context -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
  (** Given sets and A and B, return A \inter B = {}. *)

end
