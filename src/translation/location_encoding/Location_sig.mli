(* Signature of heap location encoding.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

type sort_encoding = SMT.t * SMT.t list

type 'a locs = {
  internal : 'a;
  bounds : LocationBounds0.t;
  heap_sort : HeapSort.t;

  null : SMT.t;                  (* Representation of null location *)
  sort : Sort.t;                 (* Sort of enconded locations *)
  constants : SMT.t list;        (* All locations *)
  mapping : (SMT.t * SMT.t) Sort.Map.t;
  sort_encoding : sort_encoding Sort.Map.t;
}

module type LOCATIONS_BASE = sig

  val name : string
  (** Name of the encoding. Used only for debugging. *)

  type internal

  type t = internal locs

  val init : HeapSort.t -> LocationBounds0.t -> t

  val heap_axioms : t -> SMT.t -> SMT.t

  val lemmas : t -> SMT.t

  val show : t -> string

end

module type LOCATIONS = sig

  include LOCATIONS_BASE

  (** {2 Declaration} *)

  val mk_var : t -> string -> SMT.t
  (** Create location variable. *)

  val mk_fresh_var : t -> string -> SMT.t
  (** Create fresh location variable. *)

  val mk_set_var : t -> string -> SMT.t
  (** Create location set variable. *)

  val mk_fresh_set_var : t -> string -> SMT.t
  (** Create fresh location set variable. *)


  (** {2 Accessors} *)

  val null : t -> SMT.t

  (** {2 Translation} *)

  val translate_var : t -> SL.Variable.t -> SMT.Variable.t

  val translate_term : t -> SL.Term.t -> SMT.t

  val inverse_translate : t -> SMT.Model.t -> Constant.t -> StackHeapModel.Location.t
  (** Translate an interpretation of a location to its representation in stack-heap model. *)

  (** {2 Typing} *)

  val mk_of_type : t -> SMT.t -> Sort.t -> SMT.t

  val mk_set_of_type : t -> SMT.t -> Sort.t -> SMT.t

  (** {2 Quantifiers} *)

  val mk_exists : t -> (SMT.t -> SMT.t) -> SMT.t

  val mk_forall : t -> (SMT.t -> SMT.t) -> SMT.t

  val mk_exists' : t -> int -> (SMT.t list -> SMT.t) -> SMT.t

  val mk_forall' : t -> int -> (SMT.t list -> SMT.t) -> SMT.t

  (** {2 Axioms} *)

  val axioms: t -> SL.t -> SMT.t

  val set_sort : t -> Sort.t
  (** Sort of location sets. *)

  val powerset : t -> SMT.t list list

end
