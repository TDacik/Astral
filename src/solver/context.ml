(* Solver's context
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Batteries

open SSL
open Results

type t = {

  (* Input *)
  phi : SSL.formula;
  vars : Variable.t list;

  (* Abstractions *)
  sl_graph : SL_graph.t;

  stack_bound : int * int;
  bound : int;

  (* Translation context *)
  can_scolemise : bool;
  under_star : bool;
  polarity : bool;

  locs_sort : SMT.Sort.t;
  fp_sort : SMT.Sort.t;
  heap_sort : SMT.Sort.t;

  heap : SMT.Term.t;
  locs : SMT.Term.t list;
  global_footprint : SMT.Term.t;
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

let init info locs_sort locs fp_sort global_fp heap_sort heap =
{
  phi = info.formula;
  vars = info.variables;

  sl_graph = info.sl_graph;
  bound = info.heap_bound;
  stack_bound = info.stack_bound;

  can_scolemise = true;
  under_star = false;
  polarity = true;

  locs_sort = locs_sort;
  fp_sort = fp_sort;
  heap_sort = heap_sort;

  heap = heap;
  locs = locs;
  global_footprint = global_fp;
}
