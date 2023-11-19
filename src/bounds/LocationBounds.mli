(* Location bounds.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

module SortBound : sig

  type t = {
    allocated : Int.t;
    total : Int.t;
  }

  include Datatype_sig.PRINTABLE with type t := t

  val init : int -> int -> t

  val zero : t

  val n : int -> t

  val plus : t -> t -> t

end

include Datatype_sig.MONO_MAP
  with type key := Sort.t
   and type data := SortBound.t

val allocated : Sort.t -> t -> int

val total : Sort.t -> t -> int

val chunk_size : SSL.Variable.t -> float

val chunk_size_small : SSL.Variable.t -> float

val compute : SSL.t -> SL_graph.t -> t
