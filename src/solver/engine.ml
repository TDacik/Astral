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

let run_solver ctx =
  let module Backend = (val ConfigReader.get_backend () : BACKEND) in
  let module Encoding = (val ConfigReader.get_encoding () : ENCODING) in
  match Config.SolverStrategy.get () with
    | `Auto ->
      let module S = SingleQuerySolver.Make(Encoding)(Backend) in
      S.solve ctx
    | `SingleQuery ->
      let module S = SingleQuerySolver.Make(Encoding)(Backend) in
      S.solve ctx
    | `MultiQuery ->
      let module S = SingleQuerySolver.Make(Encoding)(Backend) in
      S.solve ctx


let solve (input : Context.t) =
  Logger.debug "Normalisation\n";
  let input = normalise input in

  let sl_graph = SL_graph.compute input.phi in
  if SL_graph.has_contradiction sl_graph then
    Context.set_result `Unsat ~solved_by:"contradiction" ~unsat_core:[] input
  else match FragmentChecker.check input, Config.Unsafe.get () with
  | Error reason, false -> Context.set_result (`Unknown reason) input
  | _, _ ->
    Profiler.add "Normalisation";

    (** Small model should be computed on normalised, but non-preprocessed definition *)

    (** TODO: following is a hack for interactive mode *)
    (if Config.Interactive.get () then GlobalSID.reset_results () else ());
    let distinguishers = SID_checks.compute_distinguishers @@ GlobalSID.dependency_graph () in
    let sm = SmallModels.compute input.phi distinguishers in
    Profiler.add "Small-models";
    GlobalSID.cache := sm;
    Debug.context "input" input;

    BaseLogic.use_simplification true;
    GlobalSID.preprocess_user_definitions PredicatePreprocessing.preprocess;

    let input = Preprocessor.second_phase input in

    Profiler.add "Preprocessor";
    Logger.debug "Preprocessing finished\n";
    Logger.debug "%s\n" (ModelAdapter.show input.model_adapter);
    debug_info input;

    if SL.is_false input.phi then
      Context.set_result `Unsat input ~solved_by:"preprocessor"
    else if Config.DryRun.get () then
      Context.set_result (`Unknown "dry run") input
    else
      let res = run_solver input in
      let res' = Context.apply_model_adapter res in
      (match res'.model with None -> () | Some sh -> Debug.sl_model "model" sh);
      res'

(* TODO: Do not return just input in case of exception. *)
let solve input =
  let ctx = Context.init input in
  try solve ctx with
  | Exceptions.UnknownResult (reason, _) ->
    Context.set_result (`Unknown reason) ctx
  | Exceptions.UnsupportedFragment (reason, _) ->
    Context.set_result (`Unknown ("Unsupported fragment (" ^ reason ^ ")")) ctx
