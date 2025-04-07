(* Utility for converting Astral's input format to formats of other solvers.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Convertor_sig

module Options = CMDOptions

let process input =
  let open Convertor_sig in
  let module Convertor = (val Options.convertor () : CONVERTOR) in
  let ctx =
    Context.init input
    |> Preprocessor.remove_unused_elements ~with_vars:true
  in
  NegationNormalisation.normalise ctx.phi
  |> (fun f -> if Options.imprecise () then PreciseToImprecise.to_precise f else f)
  |> (fun f -> if Convertor.params.precise_semantics then f else PreciseToImprecise.to_imprecise f)
  |> (fun f -> if Convertor.params.supports_variadic_ops then f else RemoveVariadic.apply f)
  |> (fun f -> {ctx with phi = f})

(** Entry point *)
let () =
  let input_path = Options.parse () in
  let input = Parser.parse_file input_path in
  let input = process input in

  if Options.debug () then SL.print input.phi;
  Printexc.record_backtrace (Options.debug ());

  let module Convertor = (val Options.convertor () : CONVERTOR) in
  match Options.output_path () with
    | "" -> Format.printf "%s" (Convertor.convert input)
    | path -> Convertor.convert_and_store input (path ^ "/" ^ (Options.input_name ()))
