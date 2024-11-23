(* Stack-heap models of separation logic.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open MemoryModel
open Datatype_sig

module Location : sig

  type kind =
    | Implicit of Int.t
    | SMT of Constant.t

  type t = kind * Sort.t

  val mk : Int.t -> Sort.t -> t

  val mk_smt : Constant.t -> t

  val mk_nil : Int.t -> t

  val mk_ls : Int.t -> t

  include PRINTABLE with type t := t
  include COMPARABLE with type t := t

  include Logic_sig.SORTED with type t := t and module Sort := Sort

end


module Footprint : sig

  include Set.S with type elt := Location.t

  val disjoint_list : t list -> bool

end

module Value : sig

  type t

  val mk_struct : StructDef.t -> Location.t list -> t

  val mk_data : Constant.t -> t

  include PRINTABLE with type t := t
  include COMPARABLE with type t := t

end

module Stack : sig

  type t

  module M : MONO_MAP with type key := SL.Term.t and type data := Location.t
  (** Stack is represented as a mapping from relevant terms to locations. *)

  val empty : t

  val mk : SMT.Model.t -> M.t -> t

  val add : SL.Term.t -> Location.t -> t -> t

  val eval : t -> SL.Term.t -> Location.t

  val to_smtlib : t -> string

end

module Heap : sig

  include MONO_MAP with type key := Location.t and type data := Value.t
  (** Heap is a mapping from locations to values. *)

  val find_field : Field.t -> Location.t -> t -> Location.t

  val to_smtlib : t -> string

end

type t = private {
  stack : Stack.t;
  heap : Heap.t;

  (* Additional certificates *)
  footprints : Footprint.t SL.Map.t;   (* Mapping of sub-formulae to their footprints *)
  heaps : Heap.t SL.Map.t;             (* Mapping of sub-formulae to their heaps *)
}

val empty : t

val init :
  ?footprints: Footprint.t SL.Map.t ->
  ?heaps: Heap.t SL.Map.t ->
  Stack.t -> Heap.t -> t

val eval : t -> SL.Term.t -> Location.t

val succ_field : t -> Field.t -> Location.t -> Location.t

val domain : t -> Footprint.t

val to_smtlib : t -> string

val has_path : ?field:Field.t -> t -> src:Location.t -> dst:Location.t -> bool

val get_path : ?field:Field.t -> t -> src:Location.t -> dst:Location.t -> Location.t list
(** Compute path from src to dst (excluded) via field. If field is not specified, consider all edges. *)

val has_nested_path : t -> src:Location.t -> dst:Location.t -> sink:Location.t -> field1:Field.t -> field2:Field.t -> bool

val get_nested_path : t -> src:Location.t -> dst:Location.t -> sink:Location.t -> field1:Field.t -> field2:Field.t -> Location.t list list

val output_graph : string -> t -> unit

include PRINTABLE with type t := t
