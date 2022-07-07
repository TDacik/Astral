(* Translation of SL formulae to SMT
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Backend_sig
open Translation_sig

type result =
  | Sat of StackHeapModel.t * Results.t
  | Unsat of Results.t * SMT.term list
  | Unknown of Results.t * string

module Make (Encoding : ENCODING) (Solver : BACKEND) : sig

  val solve : SSL.t -> Results.info -> result

end
