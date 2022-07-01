(* Signature for solver's adapter
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module type SOLVER = sig

  type formula
  (** Inner representation of a formula *)

  type model
  (** Inner representation of a model *)

  type status =
    | SMT_Sat of model              (* Model *)
    | SMT_Unsat of SMT.Term.t list  (* Unsat core *)
    | SMT_Unknown of string         (* Reason *)

  val name : string
  (** Name of the solver *)

  val init : unit -> unit
  (** @raise Not_available if the solver is not installed. *)

  val translate : SMT.Term.t -> formula

  val solve : SMT.Term.t -> status

  val eval : model -> SMT.Term.t -> SMT.Term.t
  (** Evaluate term in a model *)

  val show_formula : formula -> string

  val show_model : model -> string

end
