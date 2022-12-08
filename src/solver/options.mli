(* Solver's options
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Backend_sig
open Options_sig
open Translation_sig

val parse : unit -> unit

val input_path : unit -> string

val debug : unit -> bool

val produce_models : unit -> bool

val json_output : unit -> bool

val json_output_file : unit -> string

val verify_model : unit -> bool

val unsat_core : unit -> bool

val strong_separation : unit -> bool

val list_bounds : unit -> bool

val location_bound : unit -> int option

val compute_sl_graph : unit -> bool

val ignore_unused_vars : unit -> bool

val convertor : unit -> ((module CONVERTOR) * string) option
(** Return convertor module and output path. *)

val quickcheck_runs : unit -> int

val quickcheck_store : unit -> bool

val profile : unit -> bool

val backend : unit -> (module BACKEND)

val encoding : unit -> (module BASE_ENCODING)

val quantif_elim : unit -> SMT.Term.t -> SMT.Term.t

val set_backend : string -> unit

val sl_comp : unit -> bool

val exit_usage : int -> unit
