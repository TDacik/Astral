(* Debugging module
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

val init : unit -> unit

val formula : ?suffix:string -> SSL.t -> unit

val context : Translation_context.t -> unit

val translated : ?suffix:string -> SMT.Term.t -> unit

val model : StackHeapModel.t -> unit

val smt_model : SMT.Model.t -> unit

val backend_translated : string -> unit

val backend_smt_benchmark : string -> unit

val backend_simplified : string -> unit

val backend_model : string -> unit
