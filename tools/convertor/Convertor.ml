(* Convertor to other solvers' and provers' formats
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Convertor_sig

let input = ref ""
let output_path = ref ""

let _broom = ref false
let broom () = !_broom

let _imprecise = ref false
let imprecise () = !_imprecise

let input_fn filename = match filename with
  | "" -> failwith "No input file"
  | filename -> input := filename

let input_name path =
  String.split_on_char '/' path
  |> BatList.last

let convertor = ref ""
let get_convertor () = match !convertor with
  | "cvc5" -> (module CVC5Convertor : CONVERTOR)
  | "harrsh" -> (module HarrshConvertor : CONVERTOR)
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
    ("--pure-imprecise", Arg.Set _imprecise, "Input uses imprecise semantics of pure formulae.");
  ]

let usage_msg = "astral-convertor <input> --to format [-o filename]"

let parse () = Arg.parse speclist input_fn usage_msg; !input

let preprocess_variadic cond phi =
  if not cond
  then
    let phi = RemoveVariadic.apply phi in
    phi
  else phi

let preprocess_precise cond phi =
  if not cond
  then PreciseToImprecise.to_imprecise phi
  else phi

let preprocess_imprecise phi =
  if imprecise ()
  then
    let p = PreciseToImprecise.to_precise phi in
    p
  else phi

let preprocess_broom phi =
  if broom () then
    let g = SL_graph.compute phi in
    BroomPreprocessing.apply g phi
    |> PurePreprocessing.apply
    |> SSL.normalise
    |> Simplifier.simplify
  else phi

(** Entry point *)
let () =
  let input_path = parse () in
  let module Convertor = (val get_convertor () : CONVERTOR) in
  let input = Parser.parse_file input_path in
  let phi =
    SSL.normalise input.phi
    |> preprocess_imprecise
    |> preprocess_broom
    |> preprocess_precise Convertor.precise_semantics
    |> preprocess_variadic Convertor.supports_variadic_operators
  in
  let input = {input with phi = phi} in
  match !output_path with
    | "" -> Format.printf "%s" (Convertor.convert input)
    | path -> Convertor.dump (path ^ "/" ^ input_name input_path) input
