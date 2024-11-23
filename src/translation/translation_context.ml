(* Translation context
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open HeapEncoding_sig
open Location_sig
open Encoding_context_sig

type nonrec ('locs, 'heap) t = ('locs, 'heap) t

module Make
    (Locations : LOCATIONS)
    (HeapEncoding : HEAP_ENCODING with module Locations = Locations)
  = struct

  module Locations = Locations
  module HeapEncoding = HeapEncoding

  open Encoding_context_sig
  type nonrec t = (Locations.t, HeapEncoding.t) Encoding_context_sig.t

  (* ==== Basic translation operations ==== *)

  let translate_var loc_sort x = SMT.Variable.mk (SL.Variable.show x) loc_sort

  let add_footprint ctx fp = ctx.footprints <- fp :: ctx.footprints

  let add_heap ctx heap = ctx.heaps <- heap :: ctx.heaps

  let init (context : Context.t) =
    let locs = Locations.init context.raw_input.heap_sort context.location_bounds in
    let loc_sort = locs.sort in

    let fp_sort = SMT.Sets.mk_sort loc_sort in
    let arr_sort = SMT.Array.mk_sort loc_sort loc_sort in
    let domain = SMT.Sets.mk_var "footprint0" fp_sort in

    let heap = HeapEncoding.mk context.phi context.raw_input.heap_sort locs in
    {
      phi = context.phi;
      vars = context.vars;
      location_terms = SL.get_terms context.phi;
      heap_sort = context.raw_input.heap_sort;

      smt_vars = List.map (translate_var loc_sort) context.vars;

      sl_graph = context.sl_graph;
      location_bounds = context.location_bounds;

      can_skolemise = true;
      under_star = false;
      polarity = true;

      loc_sort = loc_sort;
      fp_sort = fp_sort;

      locs = locs;
      heap = heap;

      global_footprint = domain;
      block_begin = SMT.Array.mk_var "block_begin" arr_sort;
      block_end = SMT.Array.mk_var "block_end" arr_sort;

      (* TODO *)
      footprints = [domain];
      heaps = [];
    }

end
