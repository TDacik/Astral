(* Representation of input formulae with additional data.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

type status = [ `Sat | `Unsat | `Unknown of string ]

let negate_status = function
  | `Sat -> `Unsat
  | `Unsat -> `Sat
  | `Unknown reason -> `Unknown reason

let status_is_unknown = function
  | `Sat | `Unsat -> false
  | `Unknown _ -> true

type t = {
  raw_input : ParserContext.t;     (* Raw parsed input *)

  phi : SSL.t;                     (* SSL formula after preprocessing *)
  vars : SSL.Variable.t list;      (* Location variables after preprocessing *)

  expected_status : status;        (* This may differ from raw_input.status *)

  (* Bounds *)
  sl_graph : SL_graph.t;
  bounds : Bounds.t;

  (* Statistics *)
  size : Int.t option;

  (* Results *)
  status : status option;
  model : StackHeapModel.t option;
  unsat_core : SSL.t list option;
  reason_unknown : string option;
}

let init input = {
  raw_input = input;

  phi = ParserContext.get_phi input;
  vars = ParserContext.get_sl_vars input;

  expected_status = input.expected_status;

  sl_graph = SL_graph.empty;
  bounds = Bounds.empty;

  size = None;

  status = None;
  model = None;
  unsat_core = None;
  reason_unknown = None;
}

let add_metadata input sl_graph bounds =
  {input with sl_graph = sl_graph;
              bounds = bounds}

(** {2 Setters} *)

(** TODO: keep normalised? *)
let set_preprocessed input phi vars = {input with phi = phi; vars = vars}

let set_size input size = {input with size = Some size}

let set_result status ?(model=None) ?(unsat_core=None) ?(reason=None) input =
  {input with
    status = Some status;
    model = model;
    unsat_core = unsat_core;
    reason_unknown = reason;
  }


(** {2 Getters} *)

(** Get input formula before any preprocessing. *)
let get_raw_input input =
  let phi = ParserContext.get_phi input.raw_input in
  let vars = ParserContext.get_vars input.raw_input in
  (phi, vars)

let get_input input = (input.phi, input.vars)


(** Transform satisfiability to validity of entailment *)
let transform_to_entl input = match input.phi with
  | SSL.GuardedNeg _ -> input
  | _ ->
    {input with
      phi = SSL.mk_gneg input.phi (SSL.mk_false ());
      status = Option.map negate_status input.status;
    }

(* ==== Pretty-printing ==== *)

let show_status input = match input.status with
  | None -> "none"
  | Some `Sat -> "sat"
  | Some `Unsat -> "unsat"
  | Some (`Unknown reason ) -> Format.asprintf "unknown (%s)" reason

let show_expected_status input = match input.expected_status with
  | `Sat -> "sat"
  | `Unsat -> "unsat"
  | `Unknown _ -> "unknown"
