include Datatype_sig.MONO_MAP
  with type key := Sort.t
   and type data := SortBound.t

val plus : Sort.t -> int -> t -> t
