(* Solver's options
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

val parse : unit -> unit
(** Parse command-line options and return path to the input file. *)

val interactive : unit -> bool
val set_interactive : bool -> unit

val input_path : unit -> string

val produce_models : unit -> bool

val set_produce_models : bool -> unit

val json_output : unit -> bool

val json_output_file : unit -> string

val verify_model : unit -> bool

val unsat_core : unit -> bool

val strong_separation : unit -> bool

val predicate_bounds : unit -> [`Both | `Upper | `None]

(** === Debugging === *)

val debug : unit -> bool
val set_debug : bool -> unit

val debug_dir : unit -> string
val set_debug_dir : string -> unit

val stats : unit -> bool

val unicode : unit -> bool

val dry_run : unit -> bool

val profile : unit -> bool


(** === Preprocessing === *)

val preprocessing : unit -> [`None | `Default | `Aggresive]

val antiprenexing : unit -> bool

val broom_preprocessing : unit -> bool


(** === Translation === *)

val location_bound : unit -> int option

val compute_sl_graph : unit -> bool

val ignore_unused_vars : unit -> bool

val sl_quantifiers : unit -> bool

val backend : unit -> string

val backend_options : unit -> string list

val max_footprints : unit -> int option

val max_pred_enum : unit -> int option

(** ==== Encoding ==== *)

val sets : unit -> string

val locations : unit -> string

val quantifiers : unit -> string

val set_backend : string -> unit

val semantics : unit -> [`NotSpecified | `Precise | `Imprecise]

val to_json : unit -> Yojson.Basic.t

val exit_usage : int -> unit
