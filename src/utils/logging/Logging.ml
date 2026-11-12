(* Builder for loggers.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 - 2026 *)

open Logging_sig

(** Generic logging functions .*)

let log channel ~color ~tag fmt =
  let prefix = match tag with
    | "" -> ""
    | _ -> "[" ^ tag ^ "] "
  in
  Format.kasprintf (fun msg ->
    if Unix.isatty channel
    then Format.eprintf "%s%s%s%s\n%!" color prefix msg Colors.white
    else Format.eprintf "%s%s%s\n%!" color msg Colors.white
  ) fmt

let warning fmt = log Unix.stderr ~color:Colors.yellow ~tag:"" fmt

(** Builders for loggers *)

(** Simple logger only provides log functions. *)
module Make (C : CONFIG) = struct

  let match_key () =
    let key =
      Config.DebugKey.get ()
      |> (fun str -> BatString.nreplace ~sub:"|" ~by:"\\|" ~str)
      |> BatString.lowercase_ascii
      |> Format.asprintf ".*%s.*"
    in
    let regex = Str.regexp key in
    Str.string_match regex (BatString.lowercase_ascii C.name) 0

  let debug_aux () = Config.Debug.get () || Config.Verbosity.get () >= C.level

  let debug () = debug_aux () && match_key ()

  let info = Format.fprintf

  let init path =
    if not @@ Sys.file_exists path then
      match Sys.command @@ Format.asprintf "mkdir -p %s" path with
      | 0 -> ()
      | _ -> failwith ("Cannot create directory " ^ path)

  let warning fmt = log Unix.stderr ~color:Colors.yellow ~tag:C.name fmt

  let error fmt = log Unix.stderr ~color:Colors.red ~tag:C.name fmt

  let debug fmt =
    Format.kasprintf (fun msg ->
      if debug () && not @@ Config.Interactive.get ()
      then Format.printf "[%s] %s%!" C.name msg
    ) fmt;

end

module WithOutput(C : CONFIG_WITH_PATH) = struct

  open FileUtils

  include Make(C)

  let construct_path () =
    match C.path, Config.Interactive.get () with
      | `Session _, false -> assert false
      | `Session path, true -> LoggerState.session_dir () / path
      | `Toplevel path, true -> LoggerState.session_dir () / path
      | `Toplevel path, false -> LoggerState.query_dir () / path
      | `Query path, _ -> LoggerState.query_dir () / path
      | `CurrentSubquery path, _ -> LoggerState.current_subquery_dir () / path

  (** Type represing output of object of type 'a into file given by a string path *)
  type 'a output = string -> 'a -> unit

  (** Lift show function to output. *)
  let output_from_show (show : 'a -> string) : 'a output=
    let open Out_channel in
    fun path obj -> with_open_text path (fun c -> output_string c @@ show obj)

  (** Lift channel output function to output. *)
  let output_from_channel_output (channel_out : Out_channel.t -> 'a -> unit) : 'a output =
    let open Out_channel in
    fun path obj -> with_open_text path (fun c -> channel_out c obj)

  let debug_output ~name ~suffix (output : 'a output) (obj : 'a) =
    if Config.Debug.get () then
      let path = construct_path () in
      FileUtils.mk_dir_p path;
      let full_path = (path / name) ^ suffix in
      output full_path obj
    else ()

  let debug_output_apply ~name ~suffix (apply : 'a -> 'b) (output : 'b output) (obj : 'a) =
    if Config.Debug.get () then
      let path = construct_path () in
      FileUtils.mk_dir_p path;
      let full_path = (path / name) ^ suffix in
      output full_path (apply obj)
    else ()

  let sl_formula ?source ?status name phi =
    debug_output ~name ~suffix:".smt2" (SL.output_benchmark ?source ?status) phi;
    debug_output_apply ~name ~suffix:".dot" SL.to_ast SL.output_ast phi

  let sl_benchmark ?source ?status name input =
    debug_output ~name ~suffix:".smt2" (SMTLIB_printer.output_benchmark ?source ?status) input

  let inductive_predicate ?(name : string option) pred =
    let name = Option.value ~default:(InductiveDefinition.name pred) name in
    sl_formula name @@ InductiveDefinition.instantiate_formals pred

  let input ?source ?status name input =
    debug_output ~name ~suffix:".out" (output_from_show ParserContext.show) input;
    sl_formula ?source ?status name (SL.mk_and input.assertions)

  let context ?source ?status name context =
    let open Context in
    debug_output ~name ~suffix:".out" (output_from_show ParserContext.show) context.raw_input;
    sl_formula ?source ?status name context.phi

  let smt_formula ?source ?status name phi =
    debug_output ~name ~suffix:".smt2" (SMT.output_benchmark ?source ?status) phi;
    debug_output_apply ~name ~suffix:".dot" SMT.to_ast SMT.output_ast phi

  let sl_model name model =
    debug_output ~name ~suffix:".out" StackHeapModel.dump model;
    debug_output ~name ~suffix:".dot" StackHeapModel.output_graph model

  let smt_model name model =
    debug_output ~name ~suffix:".out" SMT.Model.dump model

  let sl_graph name graph =
    debug_output ~name ~suffix:".dot" SL_graph.output_file graph

  let result result =
    debug_output ~name:"result" ~suffix:".json" Json_output.output result

  let output filename print obj =
    debug_output ~name:filename ~suffix:"" (output_from_show print) obj

  let output_apply filename apply print obj =
    debug_output_apply ~name:filename ~suffix:"" apply (output_from_show print) obj

  let generic name output obj =
    debug_output ~name ~suffix:"" (output_from_channel_output output) obj

end

