(** This module groups constructors for builtin inductive predicates.

    Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open MemoryModel

(** {1 Built-in sorts} *)

val loc_ls : Sort.t
val loc_dls : Sort.t
val loc_nls : Sort.t

(** {2 - Built-in structures} *)

val struct_ls : StructDef.t
val struct_dls : StructDef.t
val struct_nls : StructDef.t


(** {1 Built-in pointers} *)

val mk_pto_ls : SL.Term.t -> next:SL.Term.t -> SL.t

val mk_pto_dls : SL.Term.t -> next:SL.Term.t -> prev:SL.Term.t -> SL.t

val mk_pto_nls : SL.Term.t -> top:SL.Term.t -> next:SL.Term.t -> SL.t


(** {1 Built-in predicates} *)

val mk_ls : SL.Term.t -> sink:SL.Term.t -> SL.t

val mk_dls : SL.Term.t -> sink:SL.Term.t -> root':SL.Term.t -> sink':SL.Term.t -> SL.t

val mk_nls : SL.Term.t -> sink:SL.Term.t -> bottom:SL.Term.t -> SL.t

val mk_freed : SL.Term.t -> SL.t
