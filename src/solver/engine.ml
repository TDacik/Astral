(* Solver
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open SSL
open Context

open Backend_sig
open Translation_sig

module Print = Printer.Make(struct let name = "Solver" end)

(** Verify result against status specified in the input *)
let verify_status input =
  let status = Option.get input.status in
  let expected = input.raw_input.expected_status in
  status = expected || status_is_unknown status || status_is_unknown expected

let verify_model_fn sh phi = None
  (*if not @@ SSL.is_positive phi then
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
  *)
(*
let select_predicate_encoding input = match SSL.classify_fragment input.phi with
  | SymbolicHeap_SAT -> (module ListEncoding.SymbolicHeaps : PREDICATE_ENCODING)
  | _ -> Options.list_encoding ()
*)
let debug_info input = match SSL.classify_fragment input.phi with
  | Atomic -> Print.debug "Solving as atomic formula\n"
  | SymbolicHeap_SAT -> Print.debug "Solving as satisfiability in SH-fragment\n"
  | SymbolicHeap_ENTL -> Print.debug "Solving as entailment in SH-fragment\n"
  | Positive -> Print.debug "Solving as positive formula\n"
  | Arbitrary -> Print.debug "Solving as arbitrary formula\n"

let solve (raw_input : ParserContext.t) =
  let input = Context.init raw_input in
  let input = Preprocessor.first_phase input in
  let sl_graph = SL_graph.compute input.phi in
  if SL_graph.has_contradiction sl_graph then
    Context.set_result `Unsat ~unsat_core:(Some []) input
  else
    let bounds = Bounds.compute input.phi sl_graph in
    let input = Context.add_metadata input sl_graph bounds in
    let input = Preprocessor.second_phase input in
    (* Recompute bounds TODO: really? *)
    let bounds = Bounds.compute input.phi sl_graph in
    let input = Context.add_metadata input sl_graph bounds in

    Debug.out_input input;

    let module Backend = (val Options.backend () : BACKEND) in
    let module Encoding = (val Options.encoding () : ENCODING) in
    let module Translation = Translation.Make(Encoding)(Backend) in

    debug_info input;
    Translation.solve input
