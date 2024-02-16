open Location_sig

module type HEAP_ENCODING = sig

  module Locations : LOCATIONS
  (** The encoding of locations. *)

  val name : string

  type t
  (** Type of encoded heap. *)

  val mk : ?suffix:string -> SSL.t -> Locations.t -> t
  (* TODO: perhaps parameter could be somethin more general like Input.t? *)

  val next : t -> SMT.t

  val prev : t -> SMT.t

  val top : t -> SMT.t

  val inverse_translate : t -> SMT.Model.model -> SMT.Term.t -> StackHeapModel.Heap.t
  (** [inverse_translate t model domain] *)

  val axioms : t -> SMT.t

  (** {2 Auxiliary function} *)

  val mk_next : t -> SMT.t -> SMT.t

  val mk_prev : t -> SMT.t -> SMT.t

  val mk_top : t -> SMT.t -> SMT.t


  val mk_image : t -> SMT.t -> SMT.t
  (** Term representing image of heap over given domain. *)

  val mk_locs : t -> SMT.t -> SMT.t
  (** Term representing all locations of heap. *)

  val mk_eq_on_domain : t -> t -> SMT.t -> SMT.t
  (** Term representing equality of two heaps on the given domain. *)

  val mk_strongly_disjoint : t -> SMT.t list -> SMT.t list -> SMT.t

end
