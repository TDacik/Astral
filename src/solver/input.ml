(* Internal representation of the input
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

type status = [ `Sat | `Unsat | `Unknown ]

type t = {
  phi : SSL.formula list;     (* Conjunction of top-level assertions *)
  vars : SSL.Variable.t list; (* List of all declared variables *)
  status : status;            (* Expected status *)

  check_sat : bool;
  get_model : bool;
  get_unsat_core : bool;
}

let default = {
  phi = [];
  vars = [];
  status = `Unknown;

  check_sat = false;
  get_model = false;
  get_unsat_core = false;
}

let get_ssl_input input =
  let phi = SSL.mk_and input.phi in
  (phi, input.vars)

let get_status_str input = match input.status with
  | `Sat -> "sat"
  | `Unsat -> "unsat"
  | `Unknown -> "unknown"

let add_assertion psi t = {t with phi = psi :: t.phi}

let add_variables xs t = {t with vars = xs @ t.vars}

let set_status status t = {t with status = status}

let set_check_sat opt t = {t with check_sat = opt}

let set_get_model opt t = {t with get_model = opt}

let set_unsat_core opt t = {t with get_unsat_core = opt}
