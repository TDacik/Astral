(* Translation of SL formulae to SMT
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Backend_sig
open Translation_sig

module Make (Encoding : ENCODING) (Solver : BACKEND) : sig

  val solve : Context.t -> Context.t

end
