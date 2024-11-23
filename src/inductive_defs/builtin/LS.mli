open MemoryModel

(** Constructors *)

val mk_pto : SL.Term.t -> next:SL.Term.t -> SL.t

val mk : SL.Term.t -> sink:SL.Term.t -> SL.t

val mk' : ?def:StructDef.t -> SL.Term.t -> sink:SL.Term.t -> SL.t


include ID_sig.BUILTIN

val register : unit -> unit


val loc_ls : Sort.t
(** Sort of locations is singly-linked lists. *)

val struct_ls : StructDef.t

val next : Field.t

val get_target: Sort.t -> HeapSort.t -> (StructDef.t list, string) Result.t
