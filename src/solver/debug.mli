(* Debugging module
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

val init : unit -> unit

val formula : ?suffix:string -> SSL.t -> unit

val context : Context.t -> unit

val qf_phi : Z3.context * Z3.Expr.expr -> unit

val translated : string -> unit

val model : StackHeapModel.t -> unit

val smt_model : SMT.Model.t -> unit
