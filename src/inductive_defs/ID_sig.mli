open MemoryModel

type instance = SL.Term.t list * StructDef.t list

module type BUILTIN_BASE = sig

  val name : string
  (** Name of the inductive predicate. *)

  val arity : int
  (** TODO: generalize *)

  val default_instantiation : StructDef.t list

  val instantiate : HeapSort.t -> Sort.t list -> (StructDef.t list, string) Result.t
  (** Check whether ID can be instantiated with variables of given sorts. If true, return
      a list of structures describing this instance. Otherwise, return a message decsribing
      why such instantiation is not possible. *)

  val unique_footprint : bool
  (** True iff the predicate has unique footprint. *)

  val struct_defs : MemoryModel.StructDef.t list

  val heap_sort : HeapSort.t


  val term_bound : SL.t -> HeapSort.t -> SL.Term.t -> float

  val sl_graph : instance -> SL_graph0.t

  val rules : SL.Variable.t list * StructDef.t list -> SL.t list


  module Bound : sig

    type t

    val show : t -> string

    val compute : SL_graph0.t -> SL.t -> instance -> LocationBounds0.t -> t

  end

  module Translation (E : Translation_sig.ENCODING) : sig

    type translation := SMT.t * SMT.t * SMT.t list

    val translate : E.Context.t -> instance -> SMT.t -> SMT.t list -> Bound.t -> translation

  end

  (** Preprocessing *)

  val preprocess : SL_graph0.t -> SL.Term.t list -> SL.t option

  (** Model checking *)

  val model_check : instance -> StackHeapModel.t -> bool

  val compute_footprints : instance -> StackHeapModel.t -> StackHeapModel.Footprint.t

end

module type BUILTIN = sig
  include BUILTIN_BASE

  val instantiate : HeapSort.t -> SL.Term.t list -> (SL.t, string) Result.t
  (** This function may return arbitrary SL formula, not necessary containing the predicate.
      For example, ls(nil, x) will be instantiated as nil = x to prevent problems with nil. *)

end
