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

let report result =
  print_result result;
  let () = if Options.stats () then Json_output.print result else () in
  let () = if Options.json_output ()
           then Json_output.output result (Options.json_output_file ())
           else ()
  in
  let () = Profiler.finish (); if Options.profile () then Profiler.report () else () in
  ()
