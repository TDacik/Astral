(** Constructors *)

val loc_nls : Sort.t

val mk_pto : SL.Term.t -> top:SL.Term.t -> next:SL.Term.t -> SL.t

val mk : SL.Term.t -> sink:SL.Term.t -> bottom:SL.Term.t -> SL.t



val register : unit -> unit

include ID_sig.BUILTIN
