(* Solver
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open SL
open Context

open Backend_sig
open Translation_sig

module Print = Logger.Make(struct let name = "Solver" let level = 1 end)

(** Verify result against status specified in the input *)
let verify_status input =
  let status = Option.get input.status in
  let expected = input.raw_input.expected_status in
  status = expected || status_is_unknown status || status_is_unknown expected

let debug_info input = match SL.classify_fragment input.phi with
  | Atomic -> Print.debug "Solving as atomic formula\n"
  | SymbolicHeap_SAT -> Print.debug "Solving as satisfiability in SH-fragment\n"
  | SymbolicHeap_ENTL -> Print.debug "Solving as entailment in SH-fragment\n"
  | Positive -> Print.debug "Solving as positive formula\n"
  | Arbitrary -> Print.debug "Solving as arbitrary formula\n"

let solve (raw_input : ParserContext.t) =
  SID.init ();
  let input = Context.init raw_input in
  let input = Preprocessor.first_phase input in
  SID.preprocess_user_definitions PredicatePreprocessing.preprocess;
  let sl_graph = SL_graph.compute input.phi in
  if SL_graph.has_contradiction sl_graph then
    Context.set_result `Unsat ~unsat_core:(Some []) input
  else match FragmentChecker.check input with
  | Error reason -> Context.set_result (`Unknown reason) input
  | Ok () ->
    let sm = SmallModels.compute !SID.dg in
    SID.cache := sm;


    let bounds1 = LocationBounds.compute input.phi input.raw_input.heap_sort sl_graph in
    let input = Context.add_metadata input sl_graph bounds1 in

    let input = Preprocessor.second_phase input in
    let bounds2 = LocationBounds.compute input.phi input.raw_input.heap_sort sl_graph in

    (* TODO: Unfolding may increase the bound, thus we take the minimum *)
    let bounds = bounds1 in
    let sl_graph = SL_graph.compute input.phi in

    let input = Context.add_metadata input sl_graph bounds in
    Debug.out_input input;
    Debug.context input;

    let module Backend = (val Options.backend () : BACKEND) in
    let module Encoding = (val Options.encoding () : ENCODING) in
    let module Translation = Translation.Make(Encoding)(Backend) in

    debug_info input;
    if not @@ Options_base.dry_run () then Translation.solve input
    else Context.set_result (`Unknown "dry run") ~reason:(Some "dry run") input
