(* Astral -- solver for strong-separation logic
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Astral_lib

let run () =
  let input_file = Options.parse () in

  (* Debug initialisation needs to be called after options' parsing *)
  Debug.init ();

  let input = Parser.parse_file input_file in
  Printexc.record_backtrace (Options.debug ());
  Timer.add "Parsing";

  let result = Solver.run input in

  ModelChecker.verify_model result;

  if Options.json_output () then
    Json_output.output result (Options.json_output_file ())
  else ()

let () =
  Timer.add "Start";
  run ();
  Timer.finish ();
  Timer.report ()
