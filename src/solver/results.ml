(* Representation of solver's result
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

(** Extended information about input *)
type info = {
  formula : SSL.formula;
  variables : SSL.Variable.t list;
  length_graph : LengthGraph.t;
  stack_bound : int * int;
  heap_bound : int;
}

type status = [ `SAT | `UNSAT | `UNKNOWN ]

type t = {
  info : info;
  size : int;
  status : status;
  model : StackHeapModel.t option;
  model_verified : bool option;
}

let create_info formula vars g stack_bound heap_bound = {
  formula = formula;
  variables = vars;
  length_graph = g;
  stack_bound = stack_bound;
  heap_bound = heap_bound;
}

let create info sh size ?(model_verified=None) status = {
  info = info;
  size = size;
  status = status;
  model = sh;
  model_verified = model_verified;
}

let set_verdict result verdict = {result with model_verified = verdict}

let input_string res = SSL.show res.info.formula

let status_string res = match res.status with
  | `SAT -> "SAT"
  | `UNSAT -> "UNSAT"
  | `UNKNOWN -> "UNKNOWN"

let model_verified_string res = match res.model_verified with
  | None -> "--"
  | Some b -> Format.asprintf "%b" b

let stack_bound_string res =
  let (min, max) = res.info.stack_bound in
  Format.asprintf "(%d, %d)" min max

let heap_loc_bound_string res = Format.asprintf "%d" res.info.heap_bound
