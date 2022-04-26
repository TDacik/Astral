(* Translation of SL formulae to SMT
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Translation_sig

type result =
  | Sat of StackHeapModel.t * Z3.Model.model * Results.t
  | Unsat of Results.t * Z3.Expr.expr list
  | Unknown of Results.t * string

module Make (Encoding : ENCODING) : sig

  val solve : SSL.t -> Results.info -> result

end
