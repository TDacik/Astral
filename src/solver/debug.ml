(* Debugging module
 *
 * If option `--debug` is passed to Astral, it will store various data useful
 * for debugging in a directory `astral_debug` created in working directory.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Context

module Options = Options_base

let delim = String.make 70 '='

let debug_dir () = Logger_state.debug_dir ()

(** {2 Debug on stderr} *)
let out_input input =
  if Options.debug () && not @@ Options.interactive () then
    Format.printf "%s\nInput:\n  Fragment: %s\n  Bounds: %s\n%s\n"
      delim
      (SL.show_fragment @@ SL.classify_fragment input.phi)
      (LocationBounds.show input.location_bounds)
      delim

(** {2 Logging} *)

let path_ast file = (debug_dir ()) ^ "/" ^ file ^ ".dot"
let path_smt_model = (debug_dir ()) ^ "/" ^ "smt_model.out"
let path_model_dot = (debug_dir ()) ^ "/" ^ "model.dot"

let sl_graph_dot suffix = Format.asprintf "%s/sl_graph%s.dot" (debug_dir ()) suffix

let debug_out file content =
  let path = (debug_dir ()) ^ "/" ^ file in
  let channel = open_out path in
  Printf.fprintf channel "%s" content;
  close_out channel

(** Recursively remove a directory *)
let rec rm path =
  if Sys.is_directory path then begin
    Sys.readdir path
    |> Array.iter (fun f -> rm @@ Filename.concat path f);
    Sys.rmdir path
  end
  else Sys.remove path

(** Debug decorator *)
let decorate fn = fun arg ->
  if not @@ Options.debug () then ()
  else if Options.interactive () && not @@ Sys.file_exists (debug_dir ()) then
    Sys.mkdir (debug_dir ()) 0o775
  else fn arg

(** Initialize debug model *)
let init () =
  if not @@ Options.debug () then ()
  else begin
    if Sys.file_exists (Options.debug_dir ())
    then rm (Options.debug_dir ()) else ();
    Sys.mkdir (Options.debug_dir ()) 0o775
  end

let formula ?force_name ?(suffix="") phi =
  let out_file = match force_name with
    | None -> if suffix = "" then "phi" else "phi_" ^ suffix
    | Some name -> name
  in
  debug_out (out_file ^ ".out") (SL.show phi);
  let ast = SL.to_ast phi in
  SL.output_ast (path_ast out_file) ast

let inductive_pred ?(suffix="") name phi =
  let out_file = if suffix == "" then name else name ^ "_" ^ suffix in
  debug_out (out_file ^ ".out") (SL.show phi);
  let ast = SL.to_ast phi in
  SL.output_ast (path_ast out_file) ast

let input input =
  debug_out "input.txt" (ParserContext.show input);
  formula ~force_name:"input" (ParserContext.get_phi input);
  SL.output_benchmark ((debug_dir ()) ^ "/input.smt2") (ParserContext.get_phi input) `Unknown

let context context =
  SL_graph.output_file (sl_graph_dot "") context.sl_graph;
  SL_graph.output_file (sl_graph_dot "_spatial") (SL_graph.spatial_projection context.sl_graph)

let translated suffix phi =
  let out_file =
    if suffix = "" then "translated"
    else "translated_" ^ suffix
  in
  SMT.output_benchmark ((debug_dir ()) ^ "/" ^ out_file ^ ".smt2") phi `Unknown;
  let ast = SMT.to_ast phi in
  SMT.output_ast ((debug_dir ()) ^ "/" ^ out_file ^ ".dot") ast

let smt_model model = debug_out "smt_model.out" (SMT.Model.show model)

let model sh =
  debug_out "sh.out" (StackHeapModel.show sh);
  StackHeapModel.output_graph path_model_dot sh

let result result =
  Json_output.output result ((debug_dir ()) ^ "/result.json")

(* === Backend's data === *)

let backend_translated str = debug_out "backend_translated.smt2" str
let backend_input str = debug_out "solver_input.smt2" str
let backend_simplified str = debug_out "backend_simplified.smt2" str
let backend_model str = debug_out "backend_model.smt2" str
let backend_call str = debug_out "backend_call.sh" str

(** Decorated functions *)
let input      = decorate input
let context    = decorate context
let model      = decorate model
let smt_model  = decorate smt_model
let result     = decorate result

let backend_translated = decorate backend_translated
let backend_simplified = decorate backend_simplified
let backend_model      = decorate backend_model
let backend_input      = decorate backend_input
let backend_call       = decorate backend_call

let translated ?(suffix="")           = decorate (translated suffix)
let formula ?(suffix="")              = decorate (formula ~suffix)
let inductive_pred ?(suffix="") name  = decorate (inductive_pred ~suffix name)
