
open Logger_sig

type location = Toplevel | Subquery

module type S = sig
  val location : location
  val directory : string option
end

module Builder (Params : S) = struct

  open FileUtils

  (** For a filename and its suffix, construct a path according to parameters
      passed to the builder. *)
  let construct_path ~name ~suffix =
    let base_dir = match Params.directory with
      | Some dir -> dir
      | None -> ""
    in
    let dir_path = match Params.location with
      | Toplevel -> LoggerState.query_path base_dir
      | Subquery -> LoggerState.sub_query_path base_dir
    in
    init dir_path;
    dir_path ++ (name ^ suffix)

  (*let debug_print ~name ~suffix (output : string -> 'a -> unit) (obj : 'a) =
    if Options.debug () then
      let path = query_path ~name ~suffix in
      let content = print obj in
      Out_channel.with_open_text path (fun c -> Out_channel.output_string c content)
    else () *)

  let out (print : 'a -> string) : (string -> 'a -> unit) =
    let open Out_channel in
    fun path obj -> with_open_text path (fun c -> output_string c @@ print obj)

  let debug_output ~name ~suffix (output : string -> 'a -> unit) (obj : 'a) =
    if Config.Debug.get () then
      let path = construct_path ~name ~suffix in
      output path obj
    else ()

  let debug_output_apply ~name ~suffix (apply : 'a -> 'b) (output : string -> 'b -> unit) (obj : 'a) =
    if Config.Debug.get () then
      let path = construct_path ~name ~suffix in
      output path (apply obj)
    else ()

  let sl_formula ?source ?status name phi =
    debug_output ~name ~suffix:".smt2" (SL.output_benchmark ?source ?status) phi;
    debug_output_apply ~name ~suffix:".dot" SL.to_ast SL.output_ast phi

  let inductive_predicate ?(name : string option) pred =
    let name = Option.value ~default:(InductiveDefinition.name pred) name in
    sl_formula name @@ InductiveDefinition.instantiate_formals pred

  let input ?source ?status name input =
    debug_output ~name ~suffix:".out" (out ParserContext.show) input;
    sl_formula ?source ?status name (SL.mk_and input.assertions)

  let context ?source ?status name context =
    let open Context in
    debug_output ~name ~suffix:".out" (out ParserContext.show) context.raw_input;
    sl_formula ?source ?status name context.phi

  let smt_formula ?source ?status name phi =
    debug_output ~name ~suffix:".smt2" (SMT.output_benchmark ?source ?status) phi;
    debug_output_apply ~name ~suffix:".dot" SMT.to_ast SMT.output_ast phi

  let sl_model name model =
    debug_output ~name ~suffix:".out" StackHeapModel.dump model;
    debug_output ~name ~suffix:".dot" StackHeapModel.output_graph model

  let smt_model name model =
    debug_output ~name ~suffix:".out" SMT.Model.dump model

  let result result =
    debug_output ~name:"result" ~suffix:".json" Json_output.output result

  let output filename print obj =
    debug_output ~name:filename ~suffix:"" (out print) obj

  let output_apply filename apply print obj =
    debug_output_apply ~name:filename ~suffix:"" apply (out print) obj

end

(** Default output to toplevel debug directory *)
include Builder (struct let location = Toplevel let directory = None end)

(** Output to sub-directory of current query
module SubQuery = Builder (struct let location = Subquery let directory = None end)
*)

(** Output to provided directory in toplevel directory *)
module QueryDir (C : CONFIG_WITH_DIR) = struct
  include Logger.Make(C)
  include Builder(struct let location = Toplevel let directory = Some C.dirname end)
end

(** Output to provided directory in current query *)
module SubQueryDir (C : CONFIG_WITH_DIR) = struct
  include Logger.Make(C)
  include Builder(struct let location = Subquery let directory = Some C.dirname end)
end
