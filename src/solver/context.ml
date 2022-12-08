(* Solver's context
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

  locs_sort : SMT.Sort.t;
  fp_sort : SMT.Sort.t;
  heap_sort : SMT.Sort.t;

  heap : SMT.Term.t;
  locs : SMT.Term.t list;

  global_footprint : SMT.Term.t;

  mutable footprints : SMT.Term.t list;   (* List of all footprints used during translation *)
}

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

let init (input : Input.t) locs_sort locs fp_sort global_fp heap_sort heap =
{
  phi = input.phi;
  vars = input.vars;

  sl_graph = input.sl_graph;
  stack_bound = input.stack_bound;
  location_bound = input.location_bound;

  can_skolemise = true;
  under_star = false;
  polarity = true;

  locs_sort = locs_sort;
  fp_sort = fp_sort;
  heap_sort = heap_sort;

  heap = heap;
  locs = locs;
  global_footprint = global_fp;
  footprints = [global_fp];
}
