(* Solver's context
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Z3
open Batteries

open SSL
open Results

type t = {

  (* Input *)
  phi : SSL.formula;
  vars : Variable.t list;

  (* Abstractions *)
  length_graph : LengthGraph.G.t;

  stack_bound : int * int;
  bound : int;

  (* Translation context *)
  polarity : bool;

  solver : Z3.context;

  locs_sort : Z3.Sort.sort;
  footprint_sort : Z3.Sort.sort;

  heap : Expr.expr;
  locs : Expr.expr list;
  global_footprint : Expr.expr;
}

let formula_footprint context psi =
  let id = SSL.subformula_id context.phi psi in
  Format.asprintf "footprint%d" id

let formula_witness_heap context psi =
  let id = SSL.subformula_id context.phi psi in
  Format.asprintf "heap%d" id

let init info solver locs_sort locs fp_sort global_fp heap =
{
  phi = info.formula;
  vars = info.variables;

  length_graph = info.length_graph;
  bound = info.heap_bound;
  stack_bound = info.stack_bound;

  polarity = true;

  solver = solver;
  locs_sort = locs_sort;
  footprint_sort = fp_sort;

  heap = heap;
  locs = locs;
  global_footprint = global_fp;
}
