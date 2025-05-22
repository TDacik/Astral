(* Translation of SL formulae to SMT
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Backend_sig
open Translation_sig

module Make (Encoding : ENCODING) (Solver : BACKEND) : sig

  val formula_footprint : (_, _) Translation_context.t -> SL.t -> SMT.t

  val translate_var : (Encoding.Locations.t, Encoding.HeapEncoding.t) Translation_context.t -> SL.Variable.t -> SMT.Variable.t

  val translate_term : (Encoding.Locations.t, Encoding.HeapEncoding.t) Translation_context.t -> SL.Term.t -> SMT.t

  val translate : Context.t -> SMT.t

  val solve : Context.t -> Context.t

end
