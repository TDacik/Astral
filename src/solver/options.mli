(* Solver's options
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Backend_sig
open Translation_sig

val parse : unit -> string
(** Parse command-line options and return path to the input file. *)

val input_path : unit -> string

val stats : unit -> bool

val debug : unit -> bool

val dry_run : unit -> bool

val produce_models : unit -> bool

val set_produce_models : bool -> unit

val json_output : unit -> bool

val json_output_file : unit -> string

val verify_model : unit -> bool

val unsat_core : unit -> bool

val strong_separation : unit -> bool

val list_bounds : unit -> bool

val preprocessing : unit -> bool

val location_bound : unit -> int option

val compute_sl_graph : unit -> bool

val ignore_unused_vars : unit -> bool

val profile : unit -> bool

val broom_preprocessing : unit -> bool

val sl_quantifiers : unit -> bool

val backend : unit -> (module BACKEND)

val backend_options : unit -> string list

val encoding : unit -> (module BASE_ENCODING)

val list_encoding : unit -> (module LIST_ENCODING)

val quantif_elim : unit -> (Translation_context.t -> SMT.Term.t -> SMT.Term.t)

val set_backend : string -> unit

val semantics : unit -> [`NotSpecified | `Precise | `Imprecise]

val to_json : unit -> Yojson.Basic.t

val exit_usage : int -> unit
