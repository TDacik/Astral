(* Solver's options
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Solver_sig
open Options_sig

val parse : unit -> unit

val input_file : unit -> string option

val debug : unit -> bool

val json_output : unit -> bool

val json_output_file : unit -> string

val verify_model : unit -> bool

val unsat_core : unit -> bool

val abstraction : unit -> bool

val local_bounds : unit -> bool

val location_bound : unit -> int option

val incremental : unit -> bool

val convertor : unit -> (module CONVERTOR) option

val output_path : unit -> string option

val quickcheck_runs : unit -> int

val quickcheck_store : unit -> bool

val profile : unit -> bool

val quantifier_elim_method : unit -> [ `None | `Expand]

val backend : unit -> (module SOLVER)

val sl_comp : unit -> bool

val exit_usage : int -> unit
