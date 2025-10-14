(** Preprocessing *)

open InductiveDefinition

module type LOGGER = sig
  include Logger_sig.LOGGER
  val dump : InductiveDefinition.t -> string -> unit
end

(** Create Logger module *)
let make_logger pred =
  let name = name pred in
  let module Logger = struct
    include Logger.MakeWithDir(struct
      let dirname = "preds/" ^ name
      let name = name
      let level = 2
    end)
    let dump pred name =
      let name = InductiveDefinition.name pred ^ name in
      let psi = instantiate_formals pred in
      dump SL.dump (name ^ ".smt2") psi;
      let ast = SL.to_ast psi in
      dump SL.output_ast (name ^ ".dot") ast
  end
  in
  (module Logger : LOGGER)


let preprocess_cases fn pred = InductiveDefinition.map_cases fn pred

let rewrite_semantics phi = match Options_base.semantics () with
  | `NotSpecified -> phi
  | `Precise -> phi
  | `Imprecise -> PreciseToImprecise.to_precise phi

let rec repeat_until_fixpoint ~eq f x =
  let x' = f x in
  if eq x x' then x
  else repeat_until_fixpoint ~eq f x'

let normalise (pred : t) =
  let module Logger = (val make_logger pred : LOGGER) in
  Logger.dump pred "";

  let pred = preprocess_cases rewrite_semantics pred in
  let _ = Logger.dump pred "_1-semantics-rewrite" in

  let pred = preprocess_cases Inlining.inline pred in
  let _ = Logger.dump pred "_2-inlining" in
  Some pred

let preprocess (pred : t) =
  let module Logger = (val make_logger pred : LOGGER) in

  let qelim case = QuantifierElimination.apply (SL_graph.compute case) case in
  let pred = preprocess_cases qelim pred in
  Logger.dump pred "_3-quntifier-elim";

  let pred = InductiveDefinition.map Simplifier.simplify pred in
  Logger.dump pred "_4_simplifier";

  (* Needs to be last as it introduces disjunctive rules *)
  let pred = RuleAntiunification.apply pred in
  Logger.dump pred "_5_generalisation";

  let pred = InductiveDefinition.map (repeat_until_fixpoint ~eq:SL.equal @@ IntroduceIfThenElse.apply) pred in
  Logger.dump pred "_6-introduce-ite";
  Some pred
