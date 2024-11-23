open MemoryModel

(** Constructors *)

val mk_pto : SL.Term.t -> next:SL.Term.t -> prev:SL.Term.t -> SL.t

val mk : SL.Term.t -> sink:SL.Term.t -> root':SL.Term.t -> sink':SL.Term.t -> SL.t


val loc_dls : Sort.t
(** Sort of locations is doubly-linked lists. *)

val struct_dls : StructDef.t

val register : unit -> unit

include ID_sig.BUILTIN
