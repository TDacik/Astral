(* Solver's options
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Backend_sig

let usage_msg = "astral [options] <input>"

(* ==== Input ==== *)

let _input_path = ref None
let input_path () = match !_input_path with
  | Some path -> path
  | None -> failwith "No input file was specified"

let _ignore_unused_vars = ref false
let ignore_unused_vars () = !_ignore_unused_vars

(* ==== Output ==== *)

let _debug = ref false
let debug () = !_debug

let _dry_run = ref false
let dry_run () = !_dry_run

let _json_output_file = ref ""
let json_output_file () = !_json_output_file

(* ==== Additional features ==== *)

let _verify_model = ref false
let verify_model () = !_verify_model

let _unsat_core = ref false
let unsat_core () = !_unsat_core

(* ==== Translation options ==== *)

let _separation = ref "strong"
let strong_separation () = match !_separation with
  | "strong" -> true
  | "weak" -> false

let _location_bound = ref None
let set_location_bound x = _location_bound := Some x
let location_bound () = match !_location_bound with
  | None -> None
  | Some x ->
      if x > 0 then Some x
      else failwith "Location bound has to be positive integer"

let _list_bounds = ref true
let list_bounds () = !_list_bounds

let _compute_sl_graph = ref true
let compute_sl_graph () = !_compute_sl_graph

let _sl_comp = ref false
let sl_comp () = !_sl_comp

(* ==== Output options ==== *)

let _produce_models = ref false
let produce_models () = !_produce_models

(* ==== Quickcheck ==== *)

let _quickcheck_runs = ref 0
let set_quickcheck_runs x = _quickcheck_runs := x
let quickcheck_runs () = !_quickcheck_runs

let _quickcheck_store = ref false
let quickcheck_store () = !_quickcheck_store

(* ==== Profiling ==== *)

let _profile = ref false
let profile () = !_profile

(* ==== SMT Backend ==== *)

let _backend = ref "cvc5"
let _backend_options = ref ""

let backend () = match !_backend with
  | "bitwuzla" -> (module Bitwuzla_backend : BACKEND)
  | "boolector" -> (module Boolector_backend : BACKEND)
  | "cvc5" -> (module CVC5_backend : BACKEND)
  | "z3" -> (module Z3_backend : BACKEND)
  (*| "parallel" -> (module Parallel : BACKEND)*)
  | other -> failwith ("unknown backend `" ^ other ^ "`")

let set_backend = function
  | "cvc5" -> _backend := "cvc5"
  | "z3" -> _backend := "z3"
  | "boolector" -> _backend := "boolector"
  | other -> failwith ("unknown backend `" ^ other ^ "`")

let backend_options () = match !_backend_options with
  | "" -> []
  | options -> BatString.split_on_char ' ' options

(* ==== Encoding ==== *)

let _encoding = ref "sets"
let encoding () = match !_encoding with
  | "sets" -> (module Encodings.Sets : Translation_sig.BASE_ENCODING)
  | "bitvectors" -> (module Encodings.Bitvectors : Translation_sig.BASE_ENCODING)
  | other -> failwith ("unknown encoding `" ^ other ^ "`")

let _list_encoding = ref "functional"
let list_encoding () = match !_list_encoding with
  | "predicate" -> (module ListEncoding.Classic : Translation_sig.LIST_ENCODING)
  | "functional" -> (module ListEncoding.Functional : Translation_sig.LIST_ENCODING)
  | other -> failwith ("unknown list encoding `" ^ other ^ "`")

let _quantifier_elimination = ref "enum"
let quantif_elim () = match !_quantifier_elimination with
  | "none" -> QuantifierElimination.no_elim
  | "enum" -> QuantifierElimination.enumeration
  | other -> failwith ("unknown quantifier elimination method `" ^ other ^ "`")


let speclist =
  [
    ("--debug", Arg.Set _debug, "Print debug info");
    ("--produce-models", Arg.Set _produce_models, "");
    ("--verify-model", Arg.Set _verify_model, "Verify obtained model");
    ("--unsat-core", Arg.Set _unsat_core, "Print unsat core");
    ("--json-output", Arg.Set_string _json_output_file, "Store solver's result as json");
    ("--backend", Arg.Set_string _backend, "Backend SMT solver (default cvc5)");
    ("--backend-options",
      Arg.Set_string _backend_options, "Pass options to backend SMT solver");
    ("--encoding", Arg.Set_string _encoding, "Method of encoding (sets | bitvectors)");
    ("--quant-elim", Arg.Set_string _quantifier_elimination, "TODO");
    ("--no-list-bounds", Arg.Clear _list_bounds, "Do not use list-length bounds");
    ("--compute-sl-graph", Arg.Clear _compute_sl_graph, "Force location bound");
    ("--loc-bound", Arg.Int set_location_bound, "Force location bound");
    ("--separation", Arg.Set_string _separation, "Separation (weak | strong");
    ("--ignore-unused-vars", Arg.Set _ignore_unused_vars, "Ignore variables that do not occur
      in the input formula");
    ("--sl-comp", Arg.Set _sl_comp, "Preprocessing for SL-comp");

    (* Encoding *)
    ("--encoding", Arg.Set_string _encoding, "Method of encoding (sets | bitvectors)");
    ("--list-encoding", Arg.Set_string _list_encoding,
      "Method of list predicate encoding (predicate | functional)");
    ("--quant-elim", Arg.Set_string _quantifier_elimination, "TODO");

    (* Quantifiers in separation logic *)
    ("--sl-quantifiers", Arg.Set _sl_quant, "TODO");

    (* Debugging *)
    ("--debug", Arg.Set _debug, "Print debug info");
    ("--dry-run", Arg.Set _dry_run, "Only translate formula and return unknown");
    ("--profile", Arg.Set _profile, "Print profiling information");

    (* Do not show '-help' *)
    ("-help", Arg.Unit ignore, "");
  ]

let input_fn filename = match filename with
  | "" -> failwith "No input file"
  | _ -> _input_path := Some filename

let json_output () = match !_json_output_file with
  | "" -> false
  | _ -> true

let to_json () =
  `Assoc [
    "Separation",             `String !_separation;
    "SL-comp preprocessing",  `Bool !_sl_comp;
    "Backend",                `String !_backend;
    "Encoding",               `String !_encoding;
    "Quantifier elimination", `String !_quantifier_elimination
  ]

let exit_usage error =
  Arg.usage speclist usage_msg;
  exit error

(** Check whether selected backend is compatible with other options. *)
let check_backend () =
  let module Backend = (val backend () : BACKEND) in

  if not @@ Backend.is_available ()
  then failwith "Selected backend solver is not installed";

  if not Backend.supports_sets && !_encoding = "sets"
  then failwith "Selected backend solver does not support set encoding";

  if not Backend.supports_quantifiers && !_quantifier_elimination = "none"
  then failwith "Selected backend solver does not support quantifiers"

let parse () =
  Arg.parse speclist input_fn usage_msg;
  check_backend ();
  input_path ()
