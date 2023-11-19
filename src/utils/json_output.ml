(* JSON output of solver's result
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Context

module Options = Options_base

let size context = match context.size with Some x -> string_of_int x | None -> "-"

let solved_by context = match context.size with Some x -> "translation" | None -> "sl-graph"

let input_to_json context =
  `Assoc [
    "Name",                 `String (Options.input_path ());
    "# variables",          `Int (List.length context.vars);
    "Expected status",      `String (Context.show_expected_status context);
    "Size",                 `Int (SSL.size context.phi);
    (*"Location sort",        `String (Sort.show context.type_env.loc_sort);*)
    "Heap sort",            `String (Sort.show context.raw_input.heap_sort);
  ]

let json_repr context =
  `Assoc [
    "Input",                input_to_json context;
    "Solved by",            `String (solved_by context);
    "Bounds",               Bounds.to_json context.bounds;
    "Formula size",         `String (size context);
    "Status",               `String (Context.show_status context);
    "Model verified",       `String "-";
    "Profiling",            Profiler.json_repr ();
    "Options",              Options.to_json ();
  ]

let print results =
  Yojson.Basic.(pretty_to_channel stdout (json_repr results));
  Printf.printf "\n"

let output results file =
  let channel = open_out_gen [Open_creat; Open_wronly] 0o666 file in
  Yojson.Basic.(pretty_to_channel channel (json_repr results));
  close_out channel
