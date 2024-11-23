type t = {
  allocated : Int.t;
  total : Int.t;
}

include Datatype_sig.PRINTABLE with type t := t

val init : int -> int -> t

val zero : t

val n : int -> t

val plus : t -> t -> t
