(* Debugging module
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

val init : unit -> unit

(** {2 Debug on stderr} *)

val out_input : Context.t -> unit

(** {2 ...} *)

val formula : ?suffix:string -> SSL.t -> unit

val input : ParserContext.t -> unit

val context : Context.t -> unit

val translated : ?suffix:string -> SMT.Term.t -> unit

val model : StackHeapModel.t -> unit

val smt_model : SMT.Model.model -> unit

val backend_translated : string -> unit

val backend_input : string -> unit

val backend_simplified : string -> unit

val backend_model : string -> unit

val backend_call : string -> unit
