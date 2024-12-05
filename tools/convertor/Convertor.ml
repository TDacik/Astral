(* Utility for converting Astral's input format to formats of other solvers.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Convertor_sig

module Options = CMDoptions

let process input =
  let module Convertor = (val Options.convertor () : CONVERTOR) in
  ParserContext.get_phi input
  |> NegationNormalisation.normalise
  |> (fun f -> if Options.imprecise () then PreciseToImprecise.to_precise f else f)
  |> (fun f -> if Convertor.precise_semantics then f else PreciseToImprecise.to_imprecise f)
  |> (fun f -> if Convertor.supports_variadic_operators then f else RemoveVariadic.apply f)
  |> (fun f -> {(Context.init input) with phi = f})

(** Entry point *)
let () =
  let input_path = Options.parse () in
  let input = Parser.parse_file input_path in
  let input = process input in

  if Options.debug () then SL.print input.phi;

  let module Convertor = (val Options.convertor () : CONVERTOR) in
  match Options.output_path () with
    | "" -> Format.printf "%s" (Convertor.convert input)
    | path -> Convertor.dump (path ^ "/" ^ (Options.input_name ())) input
