val loc_ls : Sort.t
val loc_dls : Sort.t
val loc_nls : Sort.t


val mk_pto_ls : SL.Term.t -> next:SL.Term.t -> SL.t

val mk_pto_dls : SL.Term.t -> next:SL.Term.t -> prev:SL.Term.t -> SL.t

val mk_pto_nls : SL.Term.t -> top:SL.Term.t -> next:SL.Term.t -> SL.t


val mk_ls : SL.Term.t -> sink:SL.Term.t -> SL.t

val mk_dls : SL.Term.t -> sink:SL.Term.t -> root':SL.Term.t -> sink':SL.Term.t -> SL.t

val mk_nls : SL.Term.t -> sink:SL.Term.t -> bottom:SL.Term.t -> SL.t
