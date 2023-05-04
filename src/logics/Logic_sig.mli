(* Logic signatures
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

open Datatype_sig

type 'term node_type =
  | Var of string * Sort.t
  | Operator of 'term list * Sort.t
  | Connective of 'term list          (* TODO: rename to predicate? *)
  | Quantifier of 'term list * 'term

type 'term node_info = string * 'term node_type

module type VARIABLE = sig

  type t = string * Sort.t

  include PRINTABLE with type t := t
  include COMPARABLE with type t := t

  val get_name : t -> string

  val get_sort : t -> Sort.t

  val mk : string -> Sort.t -> t
  (** Create a variable of the given sort. *)

  val mk_fresh : string -> Sort.t -> t
  (** Create a fresh variable of the given sort. *)

  val show_with_sort : t -> string
end


(** Input signature of the [Logic.Make] functor. The signature defines a type [t] which
    corresponds to AST of formulae and a function [describe_node] that describes particular
    nodes of AST and is used to derive generic functions over formulae. *)
module type TERM = sig

  type t
  (** Type representing AST of formula. *)

  val describe_node : t -> t node_info
  (** Description of single node. *)

end


(** Output signature of the [Logic.Make] functor. *)
module type LOGIC = sig

  include TERM
  include PRINTABLE with type t := t
  include COMPARABLE with type t := t

  module Collections : COLLECTIONS with type t := t

  type node_type := t node_type
  (** Generic node type fixed to type given by AST. *)

  val node_name : t -> string
  (** Name of a node. *)

  val node_type : t -> node_type
  (** Type of a node. *)

  val get_sort : t -> Sort.t

  val free_vars : t -> t list

  val get_operands : t -> t list

  val get_all_sorts : t -> Sort.t list
  (** Return all sorts used in term. *)

  val is_constant : t -> bool

  val is_quantifier_free : t -> bool

  val size : t -> int
  (** The size of a term is the number of nodes in its AST. *)

  (** {2 Higher-order functions} *)

  val for_all : (t -> bool) -> t -> bool
  (** [for_all p f] checks whether predicate p holds for all sub-terms of f. *)

  val exists : (t -> bool) -> t -> bool
  (** [exists p f] checks whether predicate p holds for some sub-terms of f. *)


  (** Special printing functions *)

  val show_with_sort : t -> string

  val to_smtlib_bench : t -> string

end
