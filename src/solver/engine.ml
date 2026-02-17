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

let postprocess ctx =
  if ctx.is_unsound && ctx.status == Some `Unsat then
    Context.set_result (`Unknown ("unsound", "TODO")) ctx
  else if ctx.is_incomplete && ctx.status == Some `Sat then
    Context.set_result (`Unknown ("incomplete", "TODO")) ctx
  else ctx


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

  let input =
    if SL.is_quantifier_free input.phi then
      input
    else {input with quantifiers = Some "yes"}
  in

  let sl_graph = SL_graph.compute input.phi in
  if SL_graph.has_contradiction sl_graph then
    Context.set_result `Unsat ~solved_by:"contradiction" ~unsat_core:[] input
  else match FragmentChecker.check input, Config.Unsafe.get () with
  | Error reason, false -> Context.set_result (`Unknown (reason, "TODO")) input
  | _, _ ->
    Profiler.add "Normalisation";

    (** Small model should be computed on normalised, but non-preprocessed definition.
        TODO: still true? *)

    GlobalSID.normalise_user_definitions PredicatePreprocessing.normalise;
    GlobalSID.preprocess_user_definitions PredicatePreprocessing.preprocess;
    GlobalSID.cache := PredicateAnalysis.compute @@ GlobalSID.get ~original:true ();

    BaseLogic.use_simplification true;

    let input = Preprocessor.second_phase input in

    (if SL.equal SL.ff input.phi then raise @@ Exceptions.Unsat "preprocessing");

    Debug.context "input" input;

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
  try postprocess @@ solve ctx with
  | Exceptions.Unsat reason ->
    Context.set_result `Unsat ~unsat_core:[] ctx (* TODO: use this or propage through exception?  *)
  | Exceptions.UnknownResult (reason, details) ->
    Context.set_result (`Unknown (reason, details)) ctx
  | Exceptions.UnsupportedFragment (reason, details) ->
    Context.set_result (`Unknown ("Unsupported fragment (" ^ reason ^ ")", details)) ctx
