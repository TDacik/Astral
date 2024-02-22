(* Solver's options
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)


let usage_msg = "astral [options] <input>"

let _interactive = ref false
let interactive () = !_interactive
let set_interactive flag = _interactive := flag

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
let set_debug flag = _debug := flag

let _debug_dir = ref "astral_debug"
let debug_dir () = !_debug_dir
let set_debug_dir dir = _debug_dir := dir

let _dry_run = ref false
let dry_run () = !_dry_run

let _stats = ref false
let stats () = !_stats

let _unicode = ref true
let unicode () = !_unicode

let _json_output_file = ref ""
let json_output_file () = !_json_output_file

(* ==== Additional features ==== *)

let _verify_model = ref false
let verify_model () = !_verify_model

let _unsat_core = ref false
let unsat_core () = !_unsat_core

(* ==== Translation options ==== *)

let _separation = ref "weak"
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

let _predicate_bounds = ref "both"
let predicate_bounds () = match !_predicate_bounds with
  | "both" -> `Both
  | "upper" -> `Upper
  | "none" -> `None
  | other -> Utils.cmd_option_error "predicate_bounds" other

let _compute_sl_graph = ref true
let compute_sl_graph () = !_compute_sl_graph

let _semantics = ref ""
let semantics () = match !_semantics with
  | "" -> `NotSpecified
  | "precise" -> `Precise
  | "imprecise" -> `Imprecise
  | other -> failwith ("Unknown semanics '" ^ other ^ "'")

let _max_footprints = ref 100
let max_footprints () = match !_max_footprints with
  | 0 -> None
  | x -> Some x

let _max_pred_enum = ref 50
let max_pred_enum () = match !_max_pred_enum with
  | 0 -> None
  | x -> Some x

(* ==== Output options ==== *)

let _produce_models = ref false
let produce_models () = !_produce_models
let set_produce_models flag = _produce_models := flag

(* ==== Profiling ==== *)

let _profile = ref false
let profile () = !_profile

(* ==== Preprocessing ==== *)

let _preprocessing = ref "default"
let preprocessing () = match !_preprocessing with
  | "none" -> `None
  | "default" -> `Default
  | "aggresive" -> `Aggresive

let _antiprenexing = ref false
let antiprenexing () = !_antiprenexing

let _broom_preprocessing = ref false
let broom_preprocessing () = !_broom_preprocessing

let _sl_quant = ref false
let sl_quantifiers () = !_sl_quant

(* ==== SMT Backend ==== *)

let _backend = ref "auto"
let backend () = !_backend

let _backend_options = ref ""
let backend_options () = match !_backend_options with
  | "" -> []
  | options -> BatString.split_on_char ' ' options

let set_backend = function
  | "cvc5" -> _backend := "cvc5"
  | "z3" -> _backend := "z3"
  | "bitwuzla" -> _backend := "bitwuzla"
  | "boolector" -> _backend := "boolector"
  | "yices2" -> _backend := "yices2"
  | other -> failwith ("unknown backend `" ^ other ^ "`")

(* ==== Encoding ==== *)

let _locations = ref "bitvectors"
let locations () = !_locations

let _heap = ref "default"
let heap () = !_heap

let _sets = ref "bitvectors"
let sets () = !_sets

let _quantifiers = ref "path"
let quantifiers () = !_quantifiers

let set_encoding = function
  | "enum" -> _locations := "enum"; _sets := "direct"
  | "bitvectors" -> _locations := "bitvectors"; _sets := "bitvectors"
  | other -> Utils.cmd_option_error "encoding" other

let speclist =
  [
    ("--produce-models", Arg.Set _produce_models, "");
    ("--verify-model", Arg.Set _verify_model, "Verify obtained model");
    ("--unsat-core", Arg.Set _unsat_core, "Print unsat core");
    ("--json-output", Arg.Set_string _json_output_file, "Store solver's result as json");
    ("--backend", Arg.Set_string _backend, "Backend SMT solver (default cvc5)");
    ("--backend-options",
      Arg.Set_string _backend_options, "Pass options to backend SMT solver");
    ("--compute-sl-graph", Arg.Clear _compute_sl_graph, "Force location bound");
    ("--loc-bound", Arg.Int set_location_bound, "Force location bound");
    ("--separation", Arg.Set_string _separation, "Separation (weak | strong");
    ("--ignore-unused-vars", Arg.Set _ignore_unused_vars, "Ignore variables that do not occur
      in the input formula");
    ("--semantics", Arg.Set_string _semantics, "Semantics of pure terms (precise | imprecise)");

    (* Preprocessing *)
    ("--preprocessing", Arg.Set_string _preprocessing,
    "Preprocessing mode (none | default | aggresive)");
    ("--antiprenexing", Arg.Set _antiprenexing, "Use antiprenexing");

    (* Optimalisations *)
    ("--pred-bounds", Arg.Set_string _predicate_bounds, "Compute predicate bounds (all)");

    (* Encoding *)
    ("--loc-encoding", Arg.Set_string _locations,
     "Encoding of locations (enum | bitvectors");
    ("--set-encoding", Arg.Set_string _sets,
     "Encoding of sets in SMT (direct | bitvectors");
    ("--qf-encoding", Arg.Set_string _quantifiers,
     "Encoding of SMT quantifiers (direct | enum)");
    ("--encoding", Arg.String set_encoding,
     "Predefined encoding of locations and sets (enum | bitvectors)");

    ("--max-footprints", Arg.Set_int _max_footprints, "");
    ("--max-pred-enum", Arg.Set_int _max_pred_enum, "");

    (* Quantifiers in separation logic *)
    ("--sl-quantifiers", Arg.Set _sl_quant, "TODO");

    (* Debugging *)
    ("--debug",   Arg.Set _debug,   "Print debug info");
    ("--stats",   Arg.Set _stats,   "Print statistics and details after finishing");
    ("--unicode", Arg.Set _unicode, "Use unicode in output");
    ("--dry-run", Arg.Set _dry_run, "Only translate formula and return unknown");
    ("--profile", Arg.Set _profile, "Print profiling information");


    (* Hidden *)
    ("--broom", Arg.Set _broom_preprocessing, "");

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
    "Separation",              `String !_separation;
    "Semantics of pure atoms", `String !_semantics;
    "Backend",                 `String !_backend;
    (*"Encoding",                `String !_encoding;*)
    (*"Quantifier elimination",  `String !_quantifier_elimination*)
  ]

let exit_usage error =
  Arg.usage speclist usage_msg;
  exit error

let parse () = Arg.parse speclist input_fn usage_msg
