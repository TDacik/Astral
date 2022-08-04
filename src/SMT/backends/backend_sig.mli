(* Signature for solver's backend
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module type BACKEND = sig

  type formula
  (** Inner representation of a formula. *)

  type model
  (** Inner representation of a model. *)

  type status =
    | SMT_Sat of model              (* Model *)
    | SMT_Unsat of SMT.Term.t list  (* Unsat core *)
    | SMT_Unknown of string         (* Reason *)

  val name : string
  (** Name of the solver. *)

  val is_available : unit -> bool

  val init : unit -> unit
  (** @raise Not_available if the solver is not available. *)

  val translate : SMT.Term.t -> formula

  val solve : SMT.Term.t -> status

  val eval : model -> SMT.Term.t -> SMT.Term.t
  (** Evaluate term in a model. *)

  val simplify : formula -> formula

  val show_formula : formula -> string

  val show_model : model -> string

end
