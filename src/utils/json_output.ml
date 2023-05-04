(* JSON output of solver's result
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Context

let input_to_json context =
  `Assoc [
    "Name",                 `String (Options.input_path ());
    "# variables",          `Int (List.length context.vars);
    "Expected status",      `String (Context.show_expected_status context);
    "Size",                 `Int (SSL.size context.phi);
    "Location sort",        `String (Sort.show context.type_env.loc_sort);
    "Heap sort",            `String (Sort.show context.type_env.heap_sort);
  ]

let json_repr context =
  `Assoc [
    "Input",                input_to_json context;
    "Formula size",         `Int (Option.get context.size);
    "Status",               `String (Context.show_status context);
    "Model verified",       `String "-";
    "Stack bound",          `String (Context.show_stack_bound context);
    "Heap location bound",  `Int context.location_bound;
    "Times",                Timer.json_repr ();
    "Options",              Options.to_json ();
  ]

let output results file =
  let channel = open_out_gen [Open_creat; Open_wronly] 0o666 file in
  Yojson.Basic.(pretty_to_channel channel (json_repr results));
  close_out channel
