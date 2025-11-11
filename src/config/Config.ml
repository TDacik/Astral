(* Declaration of commandline parameters.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

open ParamBuilder

exception CmdOptionError = ParamBuilder.OptionError

module Version = Action(struct
  let name = "--version"
  let short_name = Some 'v'
  let help = "Print version and exit"
  let action = (fun () -> print_string @@ BuildInfo.version (); exit 0)
end)

module BackendHelp = Action(struct
  let name = "--backend-help"
  let short_name = None
  let help = "TODO"
  let action = (fun () -> BackendConfig.print (); exit 0)
end)

module BenchmarkMode = False(struct
  let name = "--benchmark-mode"
  let short_name = None
  let help = "Do not check expected status"
end)

module Interactive = struct
  let self = ref false
  let set value = self := value
  let get () = !self
end

module InputFile = struct
  let self = ref "-"
  let set value = self := value
  let get () = !self
end

(** Solver behaviour *)

module DryRun = False(struct
  let name = "--dry-run"
  let short_name = None
  let help = "Perform only translation, do not solve the result"
end)

module Unsafe = False(struct
  let name = "--unsafe"
  let short_name = None
  let help = "Turn off fragment check. Results may be unsound."
end)

module Preprocessing = True(struct
  let name = "--preprocess"
  let short_name = None
  let help = "Enable preprocessing"
end)

module ProduceModels = False(struct
  let name = "--produce-models"
  let short_name = Some 'm'
  let help = "Enable model production"
end)

module VerifyModels = False(struct
  let name = "--verify-models"
  let short_name = None
  let help = "Verify that generated model satisfies input formula"
end)

(** Output *)

module Profiling = False(struct
  let name = "--profile"
  let short_name = None
  let help = "Print profiling info"
end)

module Statistics = False(struct
  let name = "--stats"
  let short_name = None
  let help = "Print statistics"
end)

module JsonOutput = Path(struct
  let name = "--json-output"
  let short_name = None
  let help = "Store json output in provided file"
  let default = "astral_result.json"
end)

module DebugDir = Path(struct
  let name = "--debug-dir"
  let short_name = None
  let help = ""
  let default = "astral_debug"
end)

module DebugKey = String(struct
  let name = "--debug-key"
  let short_name = None
  let help = ""
  let default = ".*"
end)

(** Semantics of input *)

module UseBuiltins = True(struct
  let name = "--use-builtins"
  let short_name = None
  let help = "Use built-in inductive defintions for sort, structures and inductive predicates"
end)

module StrongSeparation = False(struct
  let name = "--strong-separation"
  let short_name = None
  let help = "Use strong-separation logic (SSL)"
end)

module ImprecisePureAtoms = False(struct
  let name = "--imprecise-pure-atoms"
  let short_name = None
  let help = "Use imprecise semantics of pure atoms, i.e., pure atoms can be satisfied on \
              arbitrary heap. By default, pure atoms (including true) can be satisfied only \
              on empty heap."
end)

(** Backend solvers *)

module Backend = Enum(struct
  let name = "--backend"
  let short_name = Some 'b'
  let help = "Backend SMT solver"

  type t = [`Auto | `Bitwuzla | `Bitwuzla_ext | `Boolector | `cvc5 | `Yices2 | `Z3 | `Z3_ext]
    [@@deriving show, enum]
  let default = `Auto
end)

module BackendOptions = String(struct
  let name = "--backend-options"
  let short_name = None
  let help = "Specify command-line options for backend solver. Only used for external solvers"
  let default = ""
end)

module BackendTimeout = PositiveInt(struct (* TODO: int option *)
  let name = "--backend-timeout"
  let short_name = None
  let help = ""
  let default = 0
end)

module IncrementalBackend = Enum(struct
  let name = "--incr-backend"
  let short_name = None
  let help = "Select backend solver for incremental queries"

  type t = [`Auto | `Bitwuzla | `Z3] [@@deriving show, enum]
  let default = `Auto
end)

module IncrementalTimeout = PositiveInt(struct
  let name = "--incr-timeout"
  let short_name = None
  let help = "Set backend solver timeout for inremental queries"
  let default = 5
end)

(** Encoding *)

module LocationEncoding = Enum(struct
  let name = "--loc-encoding"
  let short_name = None
  let help = "Encoding of heap locations"
  type t = [ `Bitvectors | `Datatype | `Direct] [@@deriving show, enum]
  let default = `Bitvectors
end)

module SetEncoding = Enum(struct
  let name = "--set-encoding"
  let short_name = None
  let help = "Encoding of set theory"
  type t = [`Bitvectors | `Direct] [@@deriving show, enum]
  let default = `Bitvectors
end)

module QuantifierEncoding = Enum(struct
  let name = "--qf-encoding"
  let short_name = None
  let help = "Encoding of quantifiers"
  type t = [`Direct | `Enum] [@@deriving show, enum]
  let default = `Enum
end)

module Encoding = Enum(struct
  let name = "--encoding"
  let short_name = Some 'e'
  let help = "TODO: Encoding"
  type t = [`Bitvectors | `Direct] [@@deriving show, enum]
  let default = `Bitvectors
end)

(** Translation *)

module FootprintLimit = PositiveInt(struct (* TODO: IntOption? *)
  let name = "--fp-limit"
  let short_name = None
  let help = "Set the maximal number of footprints tracked by translation. If this limit is exceeded, \
              separation conjunctions are translated using quantifiers."
  let default = 0
end)

module IncrementalUnfolding = True(struct
  let name = "--incr-unfolding"
  let short_name = None
  let help = "Perform incremental unfolding of predicates on righ-hand side of entailment"
end)

module UnfoldingLookahead = True(struct (* TODO *)
  let name = "--unfolding-lookahead"
  let short_name = None
  let help = "Perform look-ahead when unfolding predicate calls with determined existentials (experimental)"
end)

(** Solver strategies *)

module SolverStrategy = Enum(struct
  let name = "--solver-strategy"
  let short_name = None
  let help = "Strategy for spliting entailments"
  type t = [`Auto | `SingleQuery | `MultiQuery] [@@deriving show, enum]
  let default = `Auto
end)

module Verbosity = PositiveInt(struct
  let name = "--verbosity"
  let short_name = Some 'V'
  let help = "Verbosity level"
  let default = 0
end)

module Debug = False(struct
  let name = "--debug"
  let short_name = Some 'd'
  let help = "Print debug info"
end)

let take_file_once =
  let first_time = ref true in
  fun f ->
    if !first_time then (first_time := false; InputFile.set f)
    else failwith "Expecting exactly one input file"

let usage = "astral [options] [input-file]"

let check () = () (* TODO *)

let parse_cmdline () =
  CommandLine.parse take_file_once usage;
  InputFile.get ()

let to_json = CommandLine.to_json
