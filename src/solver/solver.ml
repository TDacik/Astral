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
  status = `Unknown || status = input.expected_status

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

let select_predicate_encoding input = match SSL.classify_fragment input.phi with
  | SymbolicHeap_SAT -> (module ListEncoding.SymbolicHeaps : LIST_ENCODING)
  | _ -> (module ListEncoding.Classic : LIST_ENCODING)

let debug_info input = match snd @@ SSL.classify_fragment input.phi with
  | SymbolicHeap_SAT -> Print.debug "Solving as satisfiability in SH-fragment\n"
  | SymbolicHeap_ENTL -> Print.debug "Solving as entailment in SH-fragment\n"
  | Atomic -> Print.debug "Solving as atomic formula\n"
  | Positive -> Print.debug "Solving as positive formula\n"
  | Arbitrary -> Print.debug "Solving as arbitrary formula\n"

let solve context =
  let context, g = Preprocessor.preprocess context in

  let phi, vars = Context.get_input context in

  (** Simplification *)
  let phi = Simplifier.simplify @@ EqualityRewritter.apply g phi in
  Debug.formula phi;

  let s_min, s_max = Bounds.stack_bound g phi vars in
  let h_bound = match Options.location_bound () with
    | None -> Bounds.location_bound phi g s_max
    | Some x -> x
  in
  let input = Context.set_bounds s_min s_max h_bound g input in
  (* Create solver module *)

  let module Backend = (val Options.backend () : BACKEND) in
  let module BaseEncoding = (val Options.encoding () : BASE_ENCODING) in
  let module ListEncoding = (val select_predicate_encoding input : LIST_ENCODING) in
  let module Encoding = (struct include BaseEncoding module ListEncoding = ListEncoding end) in
  let module Translation = Translation.Make(Encoding)(Backend) in

  debug_info input;
  let result = Translation.solve input in

let run context =
  let result = solve context in
  Timer.add "Solver";

  let res_string = Context.show_status result in

  if not @@ verify_status result then
  begin
    Print.info "Internal error: result %s (expected %s)\n"
      (Context.show_status result)
      (Context.show_expected_status result);
    exit 1
  end;

  Printf.printf "%s\n" res_string;

  if Options.produce_models () then
    match Option.get result.status with
    | `Sat -> Format.printf "%s\n" (StackHeapModel.to_smtlib @@ Option.get result.model)
    | _ -> ()
  else ();

  result

