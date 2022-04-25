(* Solver
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open SSL
open Encodings

open Results

module Print = Printer.Make(struct let name = "Solver" end)

let verify_model model phi = ()

(** If phi is positive, remove all variables that does not appear in phi *)
let normalise_vars phi vars =
  if SSL.is_positive phi then
    let phi_vars = SSL.get_vars phi in
    List.filter (fun v -> List.mem v phi_vars) vars
  else vars

let normalise phi vars =
  let phi = SSL.normalise phi in
  let vars = normalise_vars phi vars in
  (phi, vars)

let solve phi vars =
  let phi, vars = normalise phi vars in

  (* Bound computation *)
  let g =
    if Options.abstraction ()
    then LengthGraph.compute phi |> MustAllocations.refine_graph phi
    else LengthGraph.top ()
  in
  let s_min, s_max = Bounds.stack_bound g phi vars in
  let h_bound = match Options.location_bound () with
    | None -> Bounds.location_bound phi g s_max
    | Some x -> x
  in
  let info = Results.create_info phi vars g (s_min, s_max) h_bound in
  match classify_fragment phi with
    | SymbolicHeap_SAT ->
      Print.info "Solving as satisfiability in SH-fragment\n";
      TranslationSH.solve info.formula info

    | SymbolicHeap_ENTL ->
      Print.info "Solving as entailment in SH-fragment\n";
      TranslationN.solve info.formula info

    | Positive ->
      Print.info "Solving as positive formula\n";
      TranslationN.solve info.formula info

    | Arbitrary ->
      Print.info "Solving as arbitrary formula\n";
      TranslationN.solve info.formula info
