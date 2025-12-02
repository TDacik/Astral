(*
 * TODO: fix intermediate results
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Astral

let register_at_exit () =
  if Config.Profiling.get () then
    Stdlib.at_exit (fun () -> Profiler.finish (); Profiler.report ())

let print_result result =
  Format.printf "%s\n" (Context.show_status result);

  if Config.ProduceModels.get () && Option.is_some result.model then
    match Option.get result.status with
      | `Sat -> Format.printf "%s\n" (StackHeapModel.to_smtlib @@ Option.get result.model)
      | _ -> ()

let report result =
  print_result result;
  let () = if Config.Statistics.get () then Json_output.print result else () in
  let () = if Config.JsonOutput.get () != ""
           then Json_output.output (Config.JsonOutput.get ()) result
           else ()
  in
  ()
