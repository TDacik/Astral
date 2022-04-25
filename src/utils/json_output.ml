(* JSON output of solver's result
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Results

let json_repr results =
  `Assoc [
    "Name",                 `String (Option.get @@ Options.input_file ());
    "Formula",              `String (Results.input_string results);
    "Formula size",         `Int results.size;
    "# variables",          `Int (List.length @@ SSL.get_vars ~with_nil:false
                                  results.info.formula);
    "Status",               `String (Results.status_string results);
    "Model verified",       `String (Results.model_verified_string results);
    "Stack bound",          `String (Results.stack_bound_string results);
    "Heap location bound",  `String (Results.heap_loc_bound_string results);
    "Times",                Timer.json_repr ();
  ]

let output results file =
  let channel = open_out_gen [Open_creat; Open_wronly] 0o666 file in
  Yojson.Basic.(pretty_to_channel channel (json_repr results));
  close_out channel
