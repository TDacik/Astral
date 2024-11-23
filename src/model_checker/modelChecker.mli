(* Model validation for symbolic heaps.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

val check : StackHeapModel.t -> SL.t -> (bool, string) Result.t
