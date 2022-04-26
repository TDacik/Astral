(* Debugging module
 *
 * If option `--debug` is passed to Astral, it will store various data useful
 * for debugging in a directory `astral_debug` created in working directory.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Context

let debug_dir = "astral_debug"

let path_ast file = debug_dir ^ "/" ^ file ^ ".dot"
let path_smt_model = debug_dir ^ "/" ^ "smt_model.out"
let path_model_dot = debug_dir ^ "/" ^ "model.dot"

let length_graph_dot = debug_dir ^ "/" ^ "length_graph.dot"
let length_proj_dot = debug_dir ^ "/" ^ "pointer_graph.dot"

let debug_out file content =
  let path = debug_dir ^ "/" ^ file in
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
let decorate fn =
  fun arg ->
    if not @@ Options.debug () then ()
    else fn arg

(** Initialize debug model *)
let init () =
  if not @@ Options.debug () then ()
  else begin
    if Sys.file_exists debug_dir
    then rm debug_dir else ();
    Sys.mkdir debug_dir 0o775
  end

let formula suffix phi =
  let out_file =
    if suffix = "" then "phi"
    else "phi_" ^ suffix
  in
  debug_out (out_file ^ ".out") (SSL.show phi);
  SSLUtils.out_ast (path_ast out_file) phi

let context context =
  LengthGraph.output_file context.length_graph length_graph_dot;
  LengthGraph.output_file (LengthGraph.pointer_projection context.length_graph) length_proj_dot

let translated (context, phi) =
  debug_out "translated.smt2" (Z3.Expr.to_string phi);
  debug_out "unsimplfied_translated.smt2" (Z3.Expr.to_string phi);
  debug_out "z3_input.smt2" (Z3.SMT.benchmark_to_smtstring context
    "Input for solver"
    "ALL"
    "unknown"
    ""
    []
    phi
  )

let model sh =
  debug_out "sh.out" (StackHeapModel.to_string sh);
  StackHeapModel.output_graph path_model_dot sh

let smt_model model =
  debug_out "smt_model.smt2" (Z3.Model.to_string model)

(** Decorated functions *)
let context    = decorate context
let translated = decorate translated
let model      = decorate model
let smt_model  = decorate smt_model

let formula ?(suffix="") = decorate (formula suffix)
