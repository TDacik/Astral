type 'term node_type =
  | Var of string * Sort.t
  | Operator of 'term list * Sort.t
  | Connective of 'term list          (* TODO: rename to predicate? *)
  | Quantifier of 'term list * 'term

type 'term node_info = string * 'term node_type

module type VARIABLE = sig

  type t = string * Sort.t

  include Datatype_sig.PRINTABLE with type t := t
  include Datatype_sig.COMPARABLE with type t := t

  val mk : string -> Sort.t -> t
  (** Create a variable of the given sort. *)

  val mk_fresh : string -> Sort.t -> t
  (** Create a fresh variable of the given sort. *)

  val show_with_sort : t -> string
end

module type TERM = sig

  type t
  (** Type representing AST of formula. *)

  val describe_node : t -> t node_info
  (** Description of individual nodes in AST. *)

end

module type LOGIC = sig

  type t

  type node_type := t node_type
  (** Node type fixed to types given by AST. *)

  include Datatype_sig.PRINTABLE with type t := t

  val node_name : t -> string
  (** Name of a node. *)

  val node_type : t -> node_type
  (** Type of a node. *)

  val get_sort : t -> Sort.t


  val free_vars : t -> t list

  val is_constant : t -> bool

  val size : t -> int
  (* Number of nodes in the AST. *)

  val show : t -> string

  val show_with_sort : t -> string

end
