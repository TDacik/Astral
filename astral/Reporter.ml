(*
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Astral

let print_result result =
  Format.printf "%s\n" (Context.show_status result);

  if Options.produce_models () && Option.is_some result.model then
    match Option.get result.status with
      | `Sat -> Format.printf "%s\n" (StackHeapModel.to_smtlib @@ Option.get result.model)
      | _ -> ()

let print_stats result =
  (if Options.stats () then Json_output.print result else ());
  (if Options.json_output () then Json_output.output result (Options.json_output_file ()));
  ()

let print_intermediate_stats () = match !Stats.stats with
  | None -> Printf.printf ""
  | Some stats -> print_stats stats

let report result =
  print_result result

let exit_report () =
  (if Options.profile () then (Profiler.finish (); Profiler.report ()));
  (if Options.stats () then print_intermediate_stats ());
  ()

let exit_on_signal signal =
  print_endline "unknown (canceled)";
  (* Udpate in Ocaml 5.4 using Sys.int_of_signal *)
  let exit_code = match signal with
    | _ when signal = Sys.sigint -> 128 + 2
    | _ when signal = Sys.sigalrm -> 128 + 14
    | _ when signal = Sys.sigterm -> 128 + 15
    | _ -> assert false
  in
  exit exit_code

let () =
  Stdlib.at_exit exit_report;
  List.iter
    (fun signal -> Sys.set_signal signal @@ Sys.Signal_handle exit_on_signal)
    [Sys.sigalrm; Sys.sigint; Sys.sigterm]
