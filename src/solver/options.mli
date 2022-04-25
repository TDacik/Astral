(* Solver's options
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

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

val translate : unit -> string option

val output_path : unit -> string option

val quickcheck_runs : unit -> int

val quickcheck_store : unit -> bool

val profile : unit -> bool

val sl_comp : unit -> bool

val exit_usage : int -> unit
