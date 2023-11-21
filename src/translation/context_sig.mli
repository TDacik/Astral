open Location_sig
open HeapEncoding_sig

module type CONTEXT = sig

  module Locations : LOCATIONS
  module HeapEncoding : HEAP_ENCODING with module Locations = Locations
  (** Context needs to know these modules to use their type. *)

  type t = {

    (* Input *)
    phi : SSL.t;
    vars : SSL.Variable.t list;

    smt_vars : SMT.t list;

    (* Bounds *)
    sl_graph : SL_graph.t;
    bounds : Bounds.t;

    (* Translation context *)
    can_skolemise : bool;
    under_star : bool;
    polarity : bool;

    (* Translation sorts *)
    loc_sort : Sort.t;
    fp_sort : Sort.t;

    locs : Locations.t;
    heap : HeapEncoding.t;

    global_footprint : SMT.Term.t;

    (* Auxiliary translation terms *)
    mutable footprints : SMT.Term.t list;
    mutable heaps : SMT.Term.t list;
  }

  val init : Context.t -> t

end
