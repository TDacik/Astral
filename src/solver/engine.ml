(* Solver
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open SL
open Context

open Backend_sig
open Translation_sig

module Logger = Logger.Make(struct let name = "Solver" let level = 1 end)

(** Verify result against status specified in the input *)
let verify_status input =
  let status = Option.get input.status in
  let expected = input.raw_input.expected_status in
  status = expected || status_is_unknown status || status_is_unknown expected

let debug_info input = match SL.classify_fragment input.phi with
  | Atomic -> Logger.debug "Solving as atomic formula\n"
  | SymbolicHeap_SAT -> Logger.debug "Solving as satisfiability in SH-fragment\n"
  | SymbolicHeap_ENTL -> Logger.debug "Solving as entailment in SH-fragment\n"
  | Positive -> Logger.debug "Solving as positive formula\n"
  | Arbitrary -> Logger.debug "Solving as arbitrary formula\n"

let solve (input : Context.t) =
  SID.init ();
  let input = Preprocessor.first_phase input in
  SID.preprocess_user_definitions PredicatePreprocessing.normalise;

  let sl_graph = SL_graph.compute input.phi in
  if SL_graph.has_contradiction sl_graph then
    Context.set_result `Unsat ~unsat_core:[] input
  else match FragmentChecker.check input with
  | Error reason -> Context.set_result (`Unknown reason) input
  | Ok () ->
    Profiler.add "Normalisation";
    let sm = SmallModels.compute !SID.dg input.phi in
    Profiler.add "Small-models";
    SID.cache := sm;

    Debug.out_input input;
    Debug.context input;

    let bounds1 = LocationBounds.compute input.phi input.raw_input.heap_sort sl_graph in
    let input = Context.add_metadata input sl_graph bounds1 in

    BaseLogic.use_simplification true;

    SID.preprocess_user_definitions PredicatePreprocessing.preprocess;
    let input = Preprocessor.second_phase input in
    Profiler.add "Preprocessor";
    Logger.debug "%s" (ModelAdapter.show input.model_adapter);

    let bounds2 = LocationBounds.compute input.phi input.raw_input.heap_sort sl_graph in

    (* TODO: Unfolding may increase the bound, thus we take the minimum *)
    let bounds = bounds1 in
    let sl_graph = SL_graph.compute input.phi in

    let input = Context.add_metadata input sl_graph bounds in

    let module Backend = (val Options.backend () : BACKEND) in
    let module Encoding = (val Options.encoding () : ENCODING) in
    let module Translation = Translation.Make(Encoding)(Backend) in

    debug_info input;
    if not @@ Options_base.dry_run () then
      let res = Translation.solve input in
      let res' = Context.apply_model_adapter res in
      (match res'.model with None -> () | Some sh -> Debug.model sh);
      res'
    else Context.set_result (`Unknown "dry run") input

(* TODO: Do not return just input in case of exception. *)
let solve input =
  let ctx = Context.init input in
  try solve ctx with
  | Exceptions.UnknownResult (reason, _) ->
    Context.set_result (`Unknown reason) ctx
  | Exceptions.UnsupportedFragment (reason, _) ->
    Context.set_result (`Unknown ("Unsupported fragment" ^ reason)) ctx
