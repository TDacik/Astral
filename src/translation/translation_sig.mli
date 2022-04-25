(* Signature for a generic translation
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

module type MINIMAL_SET = Set_sig.MINIMAL_SET

module type SET = Set_sig.SET

module type LOCATIONS = sig

  (* {3 Functions used to build Astral's context} *)

  val mk_sort_s : Z3.context -> string -> int -> Z3.Sort.sort

  val mk_const_s : Z3.context -> Z3.Sort.sort -> string -> Z3.Expr.expr

  val enumeration : Z3.Sort.sort -> Z3.Expr.expr list

  (* {3 functions already using Astral's context} *)

  val var_to_expr : Context.t -> SSL.Variable.t -> Z3.Expr.expr

  val vars_to_exprs : Context.t -> Z3.Expr.expr list

  val location_lemmas : Context.t -> Z3.Expr.expr

  val exists : Context.t -> (Z3.Expr.expr -> Z3.Expr.expr) -> Z3.Expr.expr
  (** First-order universal quantifier. *)

  val forall : Context.t -> (Z3.Expr.expr -> Z3.Expr.expr) -> Z3.Expr.expr
  (** First-order universal quantifier. *)

  val exists2 : Context.t -> (Z3.Expr.expr -> Z3.Expr.expr) -> Z3.Expr.expr
  (** Second-order existential quantifier. *)

  val forall2 : Context.t -> (Z3.Expr.expr -> Z3.Expr.expr) -> Z3.Expr.expr
  (** Second-order universal quantifier. *)

end

type local_bound := int * int

module type LIST_ENCODING = sig

  val semantics :
    Context.t ->
    Z3.Expr.expr ->
    Z3.Expr.expr ->
    Z3.Expr.expr ->
    local_bound ->
    Z3.Expr.expr
  (** semantics context x y fp *)

  val axioms :
    Context.t ->
    Z3.Expr.expr ->
    Z3.Expr.expr ->
    Z3.Expr.expr ->
    local_bound ->
    Z3.Expr.expr
  (** axioms context x y fp *)

end

module type ENCODING = sig

  module Set : SET

  module Locations : LOCATIONS

  module ListEncoding : LIST_ENCODING

end
