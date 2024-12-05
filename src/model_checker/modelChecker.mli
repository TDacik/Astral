(* Model validation for symbolic heaps.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

type error =
  | Unsupported of string     (* Formula is in unsupported fragment *)
  | Failure of string *string (* Internal failure: exception, backtrace *)

val check : StackHeapModel.t -> SL.t -> (bool, error) Result.t
