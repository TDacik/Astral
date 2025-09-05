open MemoryModel

type t = {
  name : string;
  header : SL.Variable.t list;

  base_cases : SL.t list;
  inductive_cases : SL.t list;
}

include Datatype_sig.PRINTABLE with type t := t
include Datatype_sig.COMPARABLE with type t := t
include Datatype_sig.COLLECTIONS with type t := t

val hash : t -> int

val name : t -> string

val header : t -> SL.Variable.t list

val cases : ?refresh:bool -> ?base_only:bool -> ?params:SL.Term.t list -> t -> SL.t list
(** Return a list of formula used in definition.

    @param refresh    Refresh name of existential variables inside each
                      definition (default true)
    @param base_only  Only base cases (default false)
*)

val instantiate_rules : t -> SL.Term.t list -> SL.t list

val show : t -> string

val mk : string -> SL.Variable.t list -> SL.t -> t

val refresh : t -> t

val mk_call : t -> SL.Term.t list -> SL.t

val arity : t -> int

val fields : t -> Field.t list

val dependencies : t -> string list
(** Return names of predicates used in inductive cases. *)

val is_ite : t -> bool

val map : (SL.t -> SL.t) -> t -> t

val map_cases : (SL.t -> SL.t) -> t -> t

val to_formula : ?params:SL.Term.t list -> t -> SL.t
(** Create a call of the given inductive definitions.

    If not parameters are provided, formal parameters will be used *)


val instantiate : refresh:bool -> t -> SL.Term.t list -> SL.t

val instantiate_formals : ?refresh:bool -> t -> SL.t

val unfold_finite : t -> SL.Term.t list -> SL.t

val smt2_decl : t -> string
