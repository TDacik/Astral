(* Formula metadata
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

type status = [ `Sat | `Unsat | `Unknown ]

type t = {
  phi_orig : SSL.t list;           (* Conjunction of top-level assertions *)
  vars_orig : SSL.Variable.t list; (* List of all declared variables *)

  phi : SSL.t;                     (* SSL formula after preprocessing *)
  vars : SSL.Variable.t list;      (* Variables after preprocessing *)

  (* Metadata *)
  sl_graph : SL_graph.t;
  stack_bound : int * int;
  location_bound : int;

  (* Solver options *)
  check_sat : bool;
  get_model : bool;
  get_unsat_core : bool;

  (* Statistics *)
  size : int option;

  (* Expected values *)
  expected_status : status;
  expected_location_bound : int option;

  (* Results *)
  status : status option;
  model : StackHeapModel.t option;
  unsat_core : SSL.t list option;
  reason_unknown : string option;

}

let empty = {
  phi_orig = [];
  vars_orig = [];

  phi = SSL.mk_true ();
  vars = [];

  sl_graph = SL_graph.empty;
  stack_bound = (0, 0);
  location_bound = 0;

  check_sat = false;
  get_model = false;
  get_unsat_core = false;

  size = None;

  expected_status = `Unknown;
  expected_location_bound = None;

  status = None;
  model = None;
  unsat_core = None;
  reason_unknown = None;
}

(** Get input formula before preprocessing *)
let get_raw_input context =
  let phi = SSL.mk_and context.phi_orig in
  (phi, context.vars_orig)

let get_input context = (context.phi, context.vars)

(** Set normalised formula and variables *)
let set_normalised phi vars t = {t with phi = phi; vars = vars}

let set_bounds s_min s_max loc sl_graph t =
  {t with sl_graph = sl_graph;
          stack_bound = (s_min, s_max);
          location_bound = loc
  }

(* TODO: is the size field necessary *)
let set_statistics ?(size=None) context = {context with size = size}

let set_result status ?(model=None) ?(unsat_core=None) ?(reason=None) context =
  {context with status = Some status;
              model = model;
              unsat_core = unsat_core;
              reason_unknown = reason;
  }

let add_assertion psi t = {t with phi_orig = psi :: t.phi_orig;}

let add_variables xs t = {t with vars_orig = xs @ t.vars_orig}

let set_expected_status status t = {t with expected_status = status}

let set_check_sat t = {t with check_sat = true}

let set_get_model t = {t with get_model = true}

let set_unsat_core t = {t with get_unsat_core = true}

let set_expected_loc_bound bound t = {t with expected_location_bound = Some bound}

(* ==== Pretty-printing ==== *)

let show_status context = match context.status with
  | None -> "none"
  | Some `Sat -> "sat"
  | Some `Unsat -> "unsat"
  | Some `Unknown -> "unknown"

let show_expected_status context = match context.expected_status with
  | `Sat -> "sat"
  | `Unsat -> "unsat"
  | `Unknown -> "unknown"

let show_stack_bound context =
  let min, max = context.stack_bound in
  Format.asprintf "[%d, %d]" min max
