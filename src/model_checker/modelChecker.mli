(* Model validation for symbolic heaps.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

val check : StackHeapModel.t -> SSL.t -> bool

val verify_model : Context.t -> unit
