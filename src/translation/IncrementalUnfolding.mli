(* Incremental unfolding of inductive predicates on RHS guided by constraints on LHS.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

open Backend_sig
open Translation_sig

val used_lookahead : bool ref

module Make (Encoding : ENCODING) (Solver : BACKEND) : sig

  val unfold : Context.t -> SMT.t -> SL.t -> SL.t

end
