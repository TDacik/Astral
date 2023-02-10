(* Astral -- solver for strong-separation logic
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Astral_lib

let run () =
  let input_file = Options.parse () in
  let input = Parser.parse_file input_file in
  Timer.add "Parsing";

  Debug.init ();

  let result = Solver.solve input ~verify_model:(Options.verify_model ()) in

  if Options.json_output () then
    Json_output.output result (Options.json_output_file ())
  else ()

let () =
  Timer.add "Start";
  Printexc.record_backtrace (Options.debug ());
  run ();
  Timer.finish ();
  Timer.report ()
