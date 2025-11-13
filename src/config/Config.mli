(* Declaration of commandline parameters.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

open Param_sig

exception CmdOptionError of string

val check : unit -> unit

val parse_cmdline : ?version:string -> unit -> string

val to_json : unit -> Yojson.Basic.t


module Interactive : sig
  val set : bool -> unit
  val get : unit -> bool
end

module InputFile : sig
  val get : unit -> string
end

(** Solver behaviour *)

module BenchmarkMode : BOOL

module DryRun : BOOL

module Unsafe : BOOL

module Preprocessing : BOOL

module ProduceModels : BOOL

module VerifyModels : BOOL

(** Output *)

module Profiling : BOOL

module Statistics : BOOL

module JsonOutput : STRING

module DebugDir : STRING

module DebugKey : STRING

(** Semantics of input *)

module UseBuiltins : BOOL

module StrongSeparation : BOOL

module ImprecisePureAtoms : BOOL

(** Backend solvers *)

module Backend : ENUM
  with type t = [`Auto | `Bitwuzla | `Bitwuzla_ext | `Boolector | `cvc5 | `Yices2 | `Z3 | `Z3_ext]

module BackendOptions : STRING

module BackendTimeout : INT

module IncrementalBackend : ENUM
  with type t = [`Auto | `Bitwuzla | `Z3 ]

module IncrementalTimeout : INT

(** Encoding *)

module LocationEncoding : ENUM
  with type t = [ `Bitvectors | `Datatype | `Direct]

module SetEncoding : ENUM
  with type t = [`Bitvectors | `Direct]

module QuantifierEncoding : ENUM
  with type t = [`Direct | `Enum]

module Encoding : ENUM
  with type t = [`Bitvectors | `Sets]

(** Translation *)

module FootprintLimit : INT

module IncrementalUnfolding : BOOL

module UnfoldingLookahead : BOOL

(** Solver strategies *)

module SolverStrategy : ENUM
  with type t = [`Auto | `SingleQuery | `MultiQuery]

module Verbosity : INT

module Debug : BOOL
