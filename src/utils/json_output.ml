(* JSON output of solver's result
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Context

let json_repr context =
  `Assoc [
    "Name",                 `String (Options.input_path ());
    "Formula size",         `Int (Option.get context.size);
    "# variables",          `Int (List.length context.vars);
    "Status",               `String (Context.show_status context);
    "Expected status",      `String (Context.show_expected_status context);
    "Model verified",       `String "-";
    "Stack bound",          `String (Context.show_stack_bound context);
    "Heap location bound",  `Int context.location_bound;
    "Times",                Timer.json_repr ();
  ]

let output results file =
  let channel = open_out_gen [Open_creat; Open_wronly] 0o666 file in
  Yojson.Basic.(pretty_to_channel channel (json_repr results));
  close_out channel
