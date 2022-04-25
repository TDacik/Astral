(* Debugging module
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

val init : unit -> unit

val formula : SSL.t -> unit

val context : Context.t -> unit

val translated : Z3.context * Z3.Expr.expr -> unit

val model : StackHeapModel.t -> unit

val smt_model : Z3.Model.model -> unit
