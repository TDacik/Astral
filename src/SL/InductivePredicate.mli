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

val cases : t -> SL.t list

val show : t -> string

val mk : string -> SL.Variable.t list -> SL.t -> t

val arity : t -> int

val fields : t -> Field.t list

val dependencies : t -> string list
(** Return names of predicates used in inductive cases. *)

val refresh : t -> t

val map : (SL.t -> SL.t) -> t -> t

val map_cases : (SL.t -> SL.t) -> t -> t

val instantiate : refresh:bool -> t -> SL.Term.t list -> SL.t

val instantiate_formals : ?refresh:bool -> t -> SL.t

val unfold_finite : t -> SL.Term.t list -> SL.t

(*
val unfold : t -> SL.Term.t list -> int -> SL.t

val unfold_synchronised : SL_graph0.t -> t -> SL.Term.t list -> int -> SL.t
*)
