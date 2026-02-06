
include Datatype_sig.MONO_MAP
  with type key = Sort.t
   and type data = Int.t

val compare : t -> t -> int

val increase : t -> Sort.t -> int -> t

val decrease : t -> Sort.t -> int -> t

val increase_all : int -> t -> t

val plus : t -> t -> t

val minus : t -> t -> t

val max : t -> t -> t

val is_leq_zero : t -> Sort.t -> bool

val saturate : Sort.Set.t -> t -> t

val of_sorts : Sort.t list -> Sort.Set.t -> t

val of_location_bound : LocationBounds0.t -> t
