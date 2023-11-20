(* Command-line options
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Convertor_sig

let input = ref ""
let input_path () = !input

let _output_path = ref ""
let output_path () = !_output_path

let _broom = ref false
let broom () = !_broom

let _imprecise = ref false
let imprecise () = !_imprecise

let input_fn filename = match filename with
  | "" -> failwith "No input file"
  | filename -> input := filename

let input_name () =
  String.split_on_char '/' !input
  |> BatList.last

let _convertor = ref ""
let convertor () = match !_convertor with
  | "astral" -> (module AstralConvertor : CONVERTOR)
  | "cvc5" -> (module CVC5Convertor : CONVERTOR)
  | "harrsh" -> (module HarrshConvertor : CONVERTOR)
  | "grasshopper" -> (module GrasshopperConvertor : CONVERTOR)
  | "sloth" -> (module SlothConvertor : CONVERTOR)
  | "songbird" | "sls" -> (module SongbirdConvertor : CONVERTOR)
  | "slide" -> (module SlideConvertor : CONVERTOR)
  | "s2s" -> (module S2SConvertor : CONVERTOR)
  | other -> Astral.Utils.cmd_option_error "convertor" other

let _debug = ref false
let debug () = !_debug

let speclist = [
    ("-o", Arg.Set_string _output_path, "Set path to output directory");
    ("--to", Arg.Set_string _convertor,
     "Select output format (sloth | grass | songbird | slide)");
    ("--debug", Arg.Set _debug, "Debug info");
    ("--broom", Arg.Set _broom, "Broom preprocessing");
    ("--pure-imprecise", Arg.Set _imprecise,
      "Input uses imprecise semantics of pure formulae.");
  ]

let usage_msg = "astral-convertor <input> --to format [-o filename]"

let parse () = Arg.parse speclist input_fn usage_msg; !input
