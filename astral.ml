(* Astral -- solver for strong-separation logic
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Astral_lib

let run () =

  Options.parse ();

  if Options.quickcheck_runs () > 0 then begin
    Printf.printf "Running quickcheck\n";
    QuickCheck.run @@ Options.quickcheck_runs ();
    exit 0;
  end;

  let input_file = Options.input_path () in
  let phi, vars = SmtlibParser.parse input_file in

  Timer.add "Parsing";

  (* Translate input formula to other format and exit *)
  let _ = match Options.convertor () with
    | None -> ()
    | Some ((module Convertor), path) ->
      Printf.printf "Translating %s to sloth format\n" path;
      Convertor.dump path phi
  in

  Debug.init ();

  let result = Solver.solve phi vars ~verify_model:(Options.verify_model ()) in

  if Options.json_output () then
    Json_output.output result (Options.json_output_file ())
  else ()

let () =
  if Options.debug () then Printexc.record_backtrace true else ();
  run ();
  Timer.finish ();
  Timer.report ()
