(* Representation of computed location and predicate bounds.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

type t = {
  location_bounds : LocationBounds.t;
  predicate_bounds : PredicateBounds.Entry.t PredicateBounds.t;
}

include Datatype_sig.PRINTABLE with type t := t

val empty : t
(** Empty bound record *)

val compute : SSL.t -> SL_graph.t -> t

val sort_allocated : t -> Sort.t -> int

val sort_total : t -> Sort.t -> int

val predicate_bound : t -> SSL.t -> PredicateBounds.Entry.t

val sum : t -> int


(** Misc *)

val to_json : t -> Yojson.Basic.t
