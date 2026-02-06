(* Incremental unfolding of inductive predicates on RHS guided by constraints on LHS.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

open Backend_sig
open Translation_sig

module Make (Encoding : ENCODING) (Solver : BACKEND) : sig

  val unfold : Context.t -> UnfoldingBound.t -> SL.t -> SMT.t -> SL.t -> SL.t

end
