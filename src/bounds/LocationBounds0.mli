include Datatype_sig.MONO_MAP
  with type key := Sort.t
   and type data := SortBound.t

val init_sort : Sort.t -> int -> int -> t

val plus_n : Sort.t -> int -> t -> t

val plus : t -> t -> t

val max : t -> t -> t
