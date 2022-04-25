(* Astral -- solver for strong-separation logic
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

let run () =

  Options.parse ();

  if Options.quickcheck_runs () > 0 then begin
    Printf.printf "Running quickcheck\n";
    QuickCheck.run @@ Options.quickcheck_runs ();
    exit 0;
  end;

  let input_file = match Options.input_file () with
    | None -> Options.exit_usage 1; ""
    | Some file -> file
  in
  let phi, vars = SmtlibParser.parse input_file in

  Timer.add "Parsing";

  (* Translate input formula to other format and exit *)
  let _ = match Options.translate () with
    | None -> ()
    | Some mode ->
      let path = Option.get @@ Options.output_path () in
      begin match mode with
        | "sloth" -> begin
          Printf.printf "Translating %s to sloth format\n" path;
          SlothTranslator.dump path phi
        end
        | "grasshopper" -> begin
          Printf.printf "Translating %s to grasshopper format\n" path;
          GrasshopperTranslator.dump path phi
        end
        | "unfold" -> begin
          Printf.printf "Unfolding %s\n" path;
          SmtlibPrinter.dump path (PredicateUnfolding.unfold phi (2 * List.length vars));
        end;
      end
  in

  (* Preprocessing for SL-comp *)
  let phi =
   if Options.sl_comp () then SL_comp.preprocess phi
   else phi
  in

  Debug.init ();
  Debug.formula phi;

  let result = Solver.solve phi vars in
  Timer.add "Solver";
  let results = match result with
  | Sat (sh, model, results) ->
      Debug.model sh;
      Printf.printf "SAT";
      results
  | Unsat (results, unsat_core) ->
      Printf.printf "UNSAT\n";
      if Options.unsat_core () then begin
        Printf.printf "Unsat core:\n";
        List.iter (fun a -> Format.printf " - %s\n" (Z3.Expr.to_string a)) unsat_core
      end;
      results
  | Unknown (results, reason) ->
      Printf.printf "Unknown: %s\n" reason; results
  in

  (* Model verification *)
  let results =
    if Options.verify_model () then
      if not @@ SSL.is_positive phi
      then let _ = Printf.printf "[MC] Model checking of negative formulas is not implemented" in results
      else match result with
      | Sat (sh, _, _) ->
        let verdict = ModelChecker.check sh phi in
        if verdict
        then Printf.printf "[MC] Model verified\n"
        else Printf.printf "[MC] Incorrect model\n";
        Results.set_verdict results verdict
      | _ -> results
    else results
  in

  if Options.json_output () then
    Json_output.output results (Options.json_output_file ())
  else ()

let () =
  Printexc.record_backtrace true;
  run ();
  Timer.finish ();
  Timer.report ()
