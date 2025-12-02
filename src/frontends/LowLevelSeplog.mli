
type width := int

(** {2 Variables} *)

module Variable : sig

  type t

  val show : t -> string

  val mk : width -> string -> t

  val mk_fresh : width -> string -> t

  val get_name : t -> string

  val get_width : t -> width

end

(** {2 Terms} *)

module Term : sig

  type t

  val show : t -> string

  (** {3 Constructors} *)

  val mk_var : width -> string -> t

  val mk_fresh_var : width -> string -> t

  val mk_const : size:width -> int -> t
  (** Create a constant term representing integer at given width. *)

  val mk_block_begin : t -> t

  val mk_block_end : t -> t

  val mk_plus : t -> t -> t

  val mk_minus : t -> t -> t

  val show : t -> string

end

(** {2 Formulae} *)

type t

val show : t -> string

(** {3 Constructors} *)

val emp : t
(** The {i emp} atom. *)

val tt : t
(** The {i true} atom. *)

val mk_eq : Term.t list -> t
(** Equality of n terms.

    @raises SortError when arguments do not have the same width. *)

val mk_eq2 : Term.t -> Term.t -> t
(** Binary equality terms.

    @raises SortError when arguments do not have the same width. *)

val mk_distinct : Term.t list -> t
(** Pairwise disequality of n terms.

    @raises SortError when arguments do not have the same width. *)

val mk_distinct2 : Term.t -> Term.t -> t
(** Disequality of two terms.

    @raises SortError when arguments do not have the same width. *)

val mk_pto : Term.t -> Term.t -> t
(** [mk_pto x y] creates a poinst-to assertion {m x \mapsto y}. *)

val mk_pto_array : ?const:Bitvector.t -> size:Term.t -> Term.t -> t
(** [mk_pto x ~const ~size] creates an array poinst-to assertion {m x \mapsto const[size]}.
    The default value of [const] is {m \top}. *)

val mk_star : t list -> t
(** Separating conjunction. *)

val mk_exists : Variable.t list -> t -> t
(** Existential quantification. *)


(** {2 Satisfiability} *)

val check_sat : address_width:width -> t -> [`Sat | `Unsat | `Unknown]
