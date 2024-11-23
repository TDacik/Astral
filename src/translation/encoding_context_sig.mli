open Location_sig
open HeapEncoding_sig

type ('locs, 'heap) t = {
    (* Input *)
    phi : SL.t;
    vars : SL.Variable.t list;
    location_terms : SL.Term.t list;

    heap_sort : HeapSort.t;

    smt_vars : SMT.Variable.t list;

    (* Bounds *)
    sl_graph : SL_graph0.t;
    location_bounds : LocationBounds0.t;

    (* Translation context *)
    can_skolemise : bool;
    under_star : bool;
    polarity : bool;

    (* Translation sorts *)
    loc_sort : Sort.t;
    fp_sort : Sort.t;

    locs : 'locs;
    heap : 'heap;

    (* Symbols *)

    global_footprint : SMT.t;
    block_begin : SMT.t;
    block_end : SMT.t;

    (* Auxiliary translation terms *)
    mutable footprints : SMT.t list;
    mutable heaps : SMT.t list;
  }

module type ENCODING_CONTEXT = sig

  module Locations : LOCATIONS
  module HeapEncoding : HEAP_ENCODING

  type t

  val init : Context.t -> t

end
