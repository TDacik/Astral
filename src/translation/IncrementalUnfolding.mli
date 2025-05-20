(* Incremental unfolding of inductive predicates on RHS guided by constraints on LHS.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

open Backend_sig
open Translation_sig

module Make (Encoding : ENCODING) (Solver : BACKEND) : sig

  type footprint_map := (SMT.t list) SL.Map.t

  val unfold : Context.t -> SMT.t -> SL.t -> SL.t * footprint_map

end
