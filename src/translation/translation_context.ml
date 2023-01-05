(* Translation context
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Batteries

open SSL

type t = {

  (* Input *)
  phi : SSL.t;
  vars : Variable.t list;

  (* Abstractions *)
  sl_graph : SL_graph.t;
  stack_bound : int * int;
  location_bound : int;

  (* Translation context *)
  can_skolemise : bool;
  under_star : bool;
  polarity : bool;

  (* Translation sorts *)
  locs_sort : SMT.Sort.t;
  fp_sort : SMT.Sort.t;
  heap_sort : SMT.Sort.t;
  path_sort : SMT.Sort.t;

  (* Translation terms *)
  heap : SMT.Term.t;
  heap_prev : SMT.Term.t;
  locs : SMT.Term.t list;
  global_footprint : SMT.Term.t;

  (* Auxiliary translation terms *)
  mutable footprints : SMT.Term.t list;
  mutable heaps : SMT.Term.t list;
}

let add_footprint ctx fp = ctx.footprints <- fp :: ctx.footprints

let add_heap ctx heap = ctx.heaps <- heap :: ctx.heaps

let formula_footprint ?(physically=true) context psi =
  let id = SSL.subformula_id ~physically context.phi psi in
  Format.asprintf "footprint%d" id

let formula_witness_heap context psi =
  let id = SSL.subformula_id context.phi psi in
  Format.asprintf "heap%d" id

(** Compute powerset of list *)
let rec powerset = function
  | [] -> [[]]
  | x :: xs ->
      let ps = powerset xs in
      ps @ List.map (fun s -> x :: s) ps

let locations_powerset context = powerset context.locs

let init (context : Context.t) locs_sort locs global_fp heap heap_prev =
{
  phi = context.phi;
  vars = context.vars;

  sl_graph = context.sl_graph;
  stack_bound = context.stack_bound;
  location_bound = context.location_bound;

  can_skolemise = true;
  under_star = false;
  polarity = true;

  locs_sort = locs_sort;
  fp_sort = SMT.Set.mk_sort locs_sort;
  heap_sort = SMT.Array.mk_sort locs_sort locs_sort;
  path_sort = SMT.Sequence.mk_sort locs_sort;

  heap = heap;
  heap_prev = heap_prev;
  locs = locs;
  global_footprint = global_fp;
  footprints = [global_fp];
  heaps = [heap]
}
