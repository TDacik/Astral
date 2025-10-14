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

    Note: It is necessary to first initialize SID and predicate dependency
          graph to perform inlining correctly. Inlined predicates are removed
          by re-initializing SID.*)
let normalise input =
  GlobalSID.compute_graph ();
  GlobalSID.preprocess_user_definitions PredicatePreprocessing.normalise;
  GlobalSID.compute_graph ();
  let input = Preprocessor.first_phase input in
  input

let solve (input : Context.t) =
  Logger.debug "Normalisation\n";
  let input = normalise input in

  let sl_graph = SL_graph.compute input.phi in
  if SL_graph.has_contradiction sl_graph then
    Context.set_result `Unsat ~unsat_core:[] input
  else match FragmentChecker.check input, Options.unsafe () with
  | Error reason, false -> Context.set_result (`Unknown reason) input
  | _, _ ->
    Profiler.add "Normalisation";

    (** Small model should be computed on normalised, but non-preprocessed definition.
        TODO: still true? *)
    Debug.out_input input;

    GlobalSID.cache := PredicateAnalysis.compute @@ GlobalSID.get ();

    BaseLogic.use_simplification true;
    GlobalSID.preprocess_user_definitions PredicatePreprocessing.preprocess;

    let input, bounds = Preprocessor.second_phase input in

    (if SL.equal SL.ff input.phi then raise @@ Exceptions.Unsat "preprocessing");

    let input = Context.add_metadata input sl_graph (Option.get bounds) in (* TODO: compute and take min *)
    Debug.context input;


    Profiler.add "Preprocessor";
    Logger.debug "Preprocessing finished\n";

    Logger.debug "%s\n" (ModelAdapter.show input.model_adapter);

    let input = Context.add_metadata input sl_graph (Option.get bounds) in

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
  | Exceptions.Unsat reason ->
    Context.set_result `Unsat ~unsat_core:[] ctx (* TODO: use this or propage through exception?  *)
  | Exceptions.UnknownResult (reason, _) ->
    Context.set_result (`Unknown reason) ctx
  | Exceptions.UnsupportedFragment (reason, _) ->
    Context.set_result (`Unknown ("Unsupported fragment (" ^ reason ^ ")")) ctx
