(* Solver
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open SSL
open Encodings

module Print = Printer.Make(struct let name = "Solver" end)

let verify_model_fn sh phi =
  if not @@ SSL.is_positive phi then
    let _ = Printf.printf "[MC] Model checking of negative formulas is not implemented" in
    None
  else
    let verdict = ModelChecker.check sh phi in
    if verdict then
      let _ = Printf.printf "[MC] Model verified\n" in
      Some true
    else
      let _ = Printf.printf "[MC] Incorrect model\n" in
      Some false

(** If phi is positive, remove all variables that does not appear in phi *)
let normalise_vars phi vars =
  if SSL.is_positive phi then
    let phi_vars = SSL.get_vars phi in
    let vars = List.filter (fun v -> List.mem v phi_vars) vars in
    if List.mem Variable.Nil phi_vars then Variable.Nil :: vars
    else vars
  else vars

let normalise phi vars =
  let phi =
   if Options.sl_comp () then
     let phi = SL_comp.preprocess phi in
     let _ = Debug.formula ~suffix:"sl_comp_pre" phi in
     phi
   else phi
  in
  let phi = SSL.normalise phi in
  let vars = normalise_vars phi vars in
  (phi, vars)

let solve ?(verify_model=false) phi vars =
  Debug.formula ~suffix:"original" phi;
  let phi, vars = normalise phi vars in

  Debug.formula phi;

  (* Bound computation *)
  let g =
    if Options.abstraction ()
    then SL_graph.compute phi (*|> MustAllocations.refine_graph phi*)
    else SL_graph.empty
  in
  let s_min, s_max = Bounds.stack_bound g phi vars in
  let h_bound = match Options.location_bound () with
    | None -> Bounds.location_bound phi g s_max
    | Some x -> x
  in
  let info = Results.create_info phi vars g (s_min, s_max) h_bound in
  (* Create solver module *)

  let module Solver = (val Options.backend () : Backend_sig.BACKEND) in

  let result = match classify_fragment phi with
    | SymbolicHeap_SAT ->
      Print.debug "Solving as satisfiability in SH-fragment\n";
      let module Translation = Translation.Make(EncodingSH)(Solver) in
      Translation.solve info.formula info

    | SymbolicHeap_ENTL ->
      Print.debug "Solving as entailment in SH-fragment\n";
      let module Translation = Translation.Make(Encoding)(Solver) in
      Translation.solve info.formula info

    | Positive ->
      Print.debug "Solving as positive formula\n";
      let module Translation = Translation.Make(Encoding)(Solver) in
      Translation.solve info.formula info

    | Arbitrary ->
      Print.debug "Solving as arbitrary formula\n";
      let module Translation = Translation.Make(Encoding)(Solver) in
      Translation.solve info.formula info
  in

  Timer.add "Solver";

  match result with
  | Translation.Sat (sh, results) ->
      (*TODO: Debug.model sh;*)
      Print.info "sat\n";
      (* Model verification *)
      if verify_model then
        let verdict = verify_model_fn sh phi in
        Results.set_verdict results verdict
      else results
  | Translation.Unsat (results, unsat_core) ->
      Print.info "unsat\n";
      if Options.unsat_core () then begin
        Printf.printf "Unsat core:\n";
        List.iter (fun a -> Format.printf " - %s\n" (SMT.Term.show a)) unsat_core
      end;
      results
  | Translation.Unknown (results, reason) ->
      Printf.printf "unknown: %s\n" reason; results
