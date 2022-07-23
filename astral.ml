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
      Printf.printf "Translating %s to %s format\n" path Convertor.name;
      let phi = SSL.normalise phi in
      let g = SL_graph.empty in
      let _, s_max = Bounds.stack_bound g phi vars in
      let bound = Bounds.location_bound phi g s_max in
      let lbound = if SSL.is_symbolic_heap phi then 1 else bound in
      Convertor.dump path phi lbound;
      exit 0
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
