(* Convertor to other solvers' and provers' formats
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Convertor_sig

let input = ref ""
let output_path = ref ""

let _broom = ref false
let broom () = !_broom

let input_fn filename = match filename with
  | "" -> failwith "No input file"
  | filename -> input := filename

let input_name path =
  String.split_on_char '/' path
  |> BatList.last

let convertor = ref ""
let get_convertor () = match !convertor with
  | "grass" -> (module GrasshopperConvertor : CONVERTOR)
  | "sloth" -> (module SlothConvertor : CONVERTOR)
  | "songbird" | "sls" -> (module SongbirdConvertor : CONVERTOR)
  | "slide" -> (module SlideConvertor : CONVERTOR)
  | other -> failwith ("unknown format `" ^ other ^ "`")

let speclist = [
    ("-o", Arg.Set_string output_path, "Set path to output directory");
    ("--to", Arg.Set_string convertor,
     "Select output format (sloth | grass | songbird | slide)");
    ("--broom", Arg.Set _broom, "Broom preprocessing");
  ]

let usage_msg = "astral-convertor <input> --to format [-o filename]"

let parse () = Arg.parse speclist input_fn usage_msg; !input

(** Entry point *)
let () =
  let input_path = parse () in
  let module Convertor = (val get_convertor () : CONVERTOR) in
  let input = Parser.parse_file input_path in
  let input =
    if broom ()
    then
      let g = SL_graph.compute input.phi in
      let phi =
        BroomPreprocessing.apply g input.phi
        |> PurePreprocessing.apply
        |> SSL.normalise
        |> Simplifier.simplify
      in
      {input with phi = phi}
    else input
  in
  let input = {input with phi = SSL.normalise input.phi} in
  let input =
    if not @@ Convertor.supports_variadic_operators
    then {input with phi = RemoveVariadic.apply input.phi}
    else input
  in
  let input =
    if not @@ Convertor.precise_semantics
    then {input with phi = PreciseToImprecise.apply input.phi}
    else input
  in
  match !output_path with
    | "" -> Format.printf "%s" (Convertor.convert input)
    | path -> Convertor.dump (path ^ "/" ^ input_name input_path) input

