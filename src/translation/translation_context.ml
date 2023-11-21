(* Translation context
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL
open Location_sig

module Make (L : LOCATIONS) = struct

  module Locations = L
  module HeapEncoding = HeapEncoding.Make(L)

  type t = {

    (* Input *)
    phi : SSL.t;
    vars : Variable.t list;

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

  (* ==== Basic translation operations ==== *)

  let translate_var loc_sort x = SMT.Variable.mk (SSL.Variable.show x) loc_sort

  let add_footprint ctx fp = ctx.footprints <- fp :: ctx.footprints

  let add_heap ctx heap = ctx.heaps <- heap :: ctx.heaps

  let init (context : Context.t) =
    let locs = Locations.init context.bounds in
    let loc_sort = Locations.get_sort locs in

    let fp_sort = SMT.Set.mk_sort loc_sort in
    let domain = SMT.Set.mk_var "footprint0" fp_sort in

    let heap = HeapEncoding.mk context.phi locs in
    {
      phi = context.phi;
      vars = context.vars;

      smt_vars = List.map (translate_var loc_sort) context.vars;

      sl_graph = context.sl_graph;
      bounds = context.bounds;

      can_skolemise = true;
      under_star = false;
      polarity = true;

      loc_sort = loc_sort;
      fp_sort = fp_sort;

      locs = locs;
      heap = heap;

      global_footprint = domain;

      (* TODO *)
      footprints = [domain];
      heaps = [];
    }

end
