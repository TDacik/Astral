(* Utility for converting Astral's input format to formats of other solvers.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Convertor_sig

module Options = CMDOptions

let () =
  let input =
    Options.parse ()
    |> Parser.parse_file
    |> Context.init
    |> Preprocessor.remove_unused_elements ~with_vars:true
  in

  let module Convertor = (val Options.convertor () : CONVERTOR) in
  let module Opts = (val Options.options () : OPTIONS) in
  let module Convertor = Convertor.Instantiate(Opts) in

  Printexc.record_backtrace (Options.debug ());

  match Options.output_path () with
    | "" -> Format.printf "%s" (Convertor.convert input)
    | path -> Convertor.convert_and_store input (path ^ "/" ^ (Options.input_name ()))
