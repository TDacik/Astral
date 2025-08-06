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

let normalise (pred : t) =
  let module Logger = (val make_logger pred : LOGGER) in
  (* Before checking, we need to eliminate quantifiers *)
  if Inlining.can_be_inlined pred.name then
    let _ = Logger.debug "Removing predicate\n" in
    None
  else
    let _ = Logger.debug "Keeping predicate\n" in
   let _ = Logger.dump pred "" in

  let pred = preprocess_cases rewrite_semantics pred in
  let _ = Logger.dump pred "_3-semantics-rewrite" in

  (*let pred = refresh pred in
  Logger.dump pred "_2-refresh";
*)

  Some pred

let preprocess (pred : t) =
  let module Logger = (val make_logger pred : LOGGER) in

  let qelim case = QuantifierElimination.apply (SL_graph.compute case) case in
  let pred = preprocess_cases qelim pred in
  Logger.dump pred "_4-quntifier-elim";

  let forbidden_vars = SID.existentials pred in
  let pred = InductiveDefinition.map (IntroduceIfThenElse.apply ~forbidden_vars) pred in
  Logger.dump pred "_5-introduce-ite";
  Some pred
