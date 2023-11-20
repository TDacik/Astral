(* Solver's public API
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module Input = ParserContext

type solver = {
  backend : [`Z3 | `CVC5];
  encoding : [`Bitvectors | `Sets];

  produce_models : bool;
  dump_queries : [`None | `Brief | `Full];
}

let init ?(backend=`Z3) ?(encoding=`Sets) ?(produce_models=false) ?(dump_queries=`None) () = {
  backend = backend;
  encoding = encoding;

  produce_models = false;
  dump_queries = dump_queries;
}

let activate solver = () (* TODO *)



let solve solver phi =
  activate solver;
  let vars = SSL.get_vars phi in
  let input =
    let input = Input.empty in
    let input = Input.add_assertion input phi in
    Input.add_vars input vars
  in
  let result = Engine.solve input in
  match Option.get result.status with
  | `Sat -> `Sat (Option.get result.model)
  | `Unsat -> `Unsat
  | `Unknown reason -> `Unknown reason

exception UnknownResult of string

let lift res = function
  | `Sat _ -> res
  | `Unsat -> not res
  | `Unknown reason -> raise @@ UnknownResult reason

let check_sat solver phi = lift true @@ solve solver phi

let check_entl solver lhs rhs =
  let phi = SSL.mk_gneg lhs rhs in
  lift false @@ solve solver phi

let check_equiv solver lhs rhs =
  let phi1 = SSL.mk_gneg lhs rhs in
  let phi2 = SSL.mk_gneg rhs lhs in
  let phi = SSL.mk_or [phi1; phi2] in
  lift false @@ solve solver phi
