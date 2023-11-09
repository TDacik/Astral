(* Stack-heap models of separation logic.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Datatype_sig

module Location : sig

  type t = Int.t * Sort.t

  val mk : Int.t -> Sort.t -> t

  val mk_ls : Int.t -> t

  val mk_dls : Int.t -> t

  val mk_nls : Int.t -> t

  val mk_nil : Int.t -> t


  val get_sort : t -> Sort.t

  val show_with_sort : t -> string

  include PRINTABLE with type t := t
  include COMPARABLE with type t := t

end


module Footprint : Set.S with type elt := Location.t


module Value : sig

  type t =
    | LS of Location.t
    | DLS of Location.t * Location.t
    | NLS of Location.t * Location.t

  val mk_ls : Location.t -> t

  val mk_dls : next:Location.t -> prev:Location.t -> t

  val mk_nls : next:Location.t -> top:Location.t -> t

  include PRINTABLE with type t := t
  include COMPARABLE with type t := t

end

module Stack : sig

  include MONO_MAP with type key := SSL.Variable.t and type data := Location.t
  (** Stack is a mapping from variables to locations. *)

  val to_smtlib : t -> string

end

module Heap : sig

  include MONO_MAP with type key := Location.t and type data := Value.t
  (** Heap is a mapping from locations to values. *)

  val add_ls : t -> src:Location.t -> next:Location.t -> t

  val add_dls : t -> src:Location.t -> next:Location.t -> prev:Location.t -> t

  val add_nls : t -> src:Location.t -> next:Location.t -> top:Location.t -> t

  val find_field : SSL.Field.t -> Location.t -> t -> Location.t

  val to_smtlib : t -> string

end

type t = private {
  stack : Stack.t;
  heap : Heap.t;

  (* Additional certificates *)
  footprints : Footprint.t SSL.Map.t;   (* Mapping of sub-formulae to their footprints *)
  heaps : Heap.t SSL.Map.t;             (* Mapping of sub-formulae to their heaps *)
}

val empty : t

val init :
  ?footprints: Footprint.t SSL.Map.t ->
  ?heaps: Heap.t SSL.Map.t ->
  Stack.t -> Heap.t -> t

val domain : t -> Footprint.t

val to_smtlib : t -> string

val has_path : t -> src:Location.t -> dst:Location.t -> bool

val get_path : t -> src:Location.t -> dst:Location.t -> Location.t list

val output_graph : string -> t -> unit

include PRINTABLE with type t := t
