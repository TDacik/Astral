(* Astral -- solver for strong-separation logic
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Astral
open Context

(** Verify result against status specified in the input. *)
let verify_status result =
  let status = Option.get result.status in
  let expected = result.raw_input.expected_status in
  status = expected || status_is_unknown status || status_is_unknown expected

let print_result result =
  Format.printf "%s\n" (Context.show_status result);

  if Options_base.produce_models () && Option.is_some result.model then
    match Option.get result.status with
      | `Sat -> Format.printf "%s\n" (StackHeapModel.to_smtlib @@ Option.get result.model)
      | _ -> ()

let check_result result =
  if not @@ verify_status result then
    Utils.internal_error
      ~backtrace:false
      ("Expected status is " ^ Context.show_expected_status result)

let verify_model result =
  if Options_base.verify_model () && Option.is_some result.model
  then ModelChecker.verify_model result
  else ()

let print_stats result =
  if Options_base.stats ()
  then Json_output.print result
  else ()

let print_profile_info () =
  if Options_base.profile ()
  then Profiler.report ()
  else ()

let run () =
  Profiler.add "Start";
  let input_file = Options.parse () in

  (* Debug initialisation needs to be called after options' parsing *)
  Debug.init ();
  Printexc.record_backtrace (Options_base.debug ());

  let input = Parser.parse_file input_file in
  Debug.input input;
  Profiler.add "Parsing";

  let result = Engine.solve input in
  Profiler.add "Solver";

  print_result result;
  check_result result;

  verify_model result;
  Profiler.add "Model checker";

  print_stats result;
  Debug.result result;

  if Options_base.json_output () then
    Json_output.output result (Options_base.json_output_file ())
  else ();

  Profiler.finish ();
  print_profile_info ()

let () = run ()
