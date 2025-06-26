(* Solver
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open SL
open Context

open Backend_sig
open Translation_sig

module Logger = Logger.Make(struct let name = "Solver" let level = 1 end)

let debug_info input = match SL.classify_fragment input.phi with
  | Atomic -> Logger.debug "Solving as atomic formula\n"
  | SymbolicHeap_SAT -> Logger.debug "Solving as satisfiability in SH-fragment\n"
  | SymbolicHeap_ENTL -> Logger.debug "Solving as entailment in SH-fragment\n"
  | Positive -> Logger.debug "Solving as positive formula\n"
  | Arbitrary -> Logger.debug "Solving as arbitrary formula\n"

(** Apply necessary transformations to input formula and SID.

    Note: Normalisation needs to be run on formula first to correctly handle inlining
          (first inline and the remove inlined IDs from SID). *)
let normalise input =
  let input = Preprocessor.first_phase input in
  SID.preprocess_user_definitions PredicatePreprocessing.normalise;
  SID.init ();
  input

let solve (input : Context.t) =
  let input = normalise input in

  let sl_graph = SL_graph.compute input.phi in
  if SL_graph.has_contradiction sl_graph then
    Context.set_result `Unsat ~unsat_core:[] input
  else match FragmentChecker.check input, Options.unsafe () with
  | Error reason, false -> Context.set_result (`Unknown reason) input
  | _, _ ->
    Profiler.add "Normalisation";

    (** Small model should be computed on normalised, but non-preprocessed definition *)
    let distinguishers = SID_checks.compute_distinguishers !SID.dg in
    let sm = SmallModels.compute input.phi distinguishers in
    Profiler.add "Small-models";
    SID.cache := sm;
    SID.distinguishers := distinguishers;

    Debug.out_input input;

    SID.preprocess_user_definitions PredicatePreprocessing.preprocess;

    let bounds = LocationBounds.compute input.phi input.raw_input.heap_sort sl_graph in
    let input = Context.add_metadata input sl_graph bounds in

    Debug.context input;

    BaseLogic.use_simplification true;

    let input = Preprocessor.second_phase input in
    Profiler.add "Preprocessor";
    Logger.debug "%s" (ModelAdapter.show input.model_adapter);

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
    Context.set_result (`Unknown ("Unsupported fragment (" ^ reason ^ ")")) ctx
