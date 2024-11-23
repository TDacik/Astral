(* Location bounds.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

include Datatype_sig.MONO_MAP
  with type key := Sort.t
   and type data := SortBound.t
   and type t = LocationBounds0.t

val allocated : Sort.t -> t -> int

val total : Sort.t -> t -> int

val term_bound : HeapSort.t -> SL_graph0.t -> SL.t -> SL.Term.t -> float

val compute : SL.t -> HeapSort.t -> SL_graph0.t -> t

val to_json : t -> Yojson.Basic.t

val sum : t -> int
