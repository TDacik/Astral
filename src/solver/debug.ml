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

let sl_graph_dot = debug_dir ^ "/" ^ "sl_graph.dot"

let debug_out file content =
  let path = debug_dir ^ "/" ^ file in
  let channel = open_out path in
  Printf.fprintf channel "%s" content;
  close_out channel

let query_counter = ref 0

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
  SL_graph.output_file context.sl_graph sl_graph_dot

let qf_phi (context, phi) =
  debug_out "phi_base.smt2" (Z3.Expr.to_string phi)

let translated phi =
  debug_out "translated.out" (SMT.Term.show phi)

let smt_model model = debug_out "smt_model.out" (SMT.Model.show model)

let model sh =
  debug_out "sh.out" (StackHeapModel.to_string sh);
  StackHeapModel.output_graph path_model_dot sh


(* === Backend's data === *)

let backend_translated str = debug_out "backend_translated.smt2" str
let backend_smt_benchmark str = debug_out "solver_input.smt2" str
let backend_simplified str = debug_out "backend_simplified.smt2" str
let backend_model str = debug_out "backend_model.smt2" str


(** Decorated functions *)
let context    = decorate context
let qf_phi     = decorate qf_phi
let translated = decorate translated
let model      = decorate model
let smt_model  = decorate smt_model

let backend_translated    = decorate backend_translated
let backend_simplified    = decorate backend_simplified
let backend_model         = decorate backend_model
let backend_smt_benchmark = decorate backend_smt_benchmark

let formula ?(suffix="") = decorate (formula suffix)
