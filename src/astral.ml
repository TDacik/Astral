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
  match Options.convertor () with
    | None -> ()
    | Some (module Convertor) ->
      Printf.printf "Translating %s to sloth format\n" "TODO.smt2";
      Convertor.dump "TODO" phi
  ;



  Debug.init ();

  let result = Solver.solve phi vars ~verify_model:(Options.verify_model ()) in

  if Options.json_output () then
    Json_output.output result (Options.json_output_file ())
  else ()

let () =
  Printexc.record_backtrace true;
  run ();
  Timer.finish ();
  Timer.report ()
