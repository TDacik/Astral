(* Signature for solver's backend
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

module type BACKEND = sig

  type formula
  (** Internal representation of a formula. *)

  type model
  (** Internal representation of a model. *)

  type status =
    | SMT_Sat of model              (* Model *)
    | SMT_Unsat of SMT.Term.t list  (* Unsat core *)
    | SMT_Unknown of string         (* Reason *)

  val name : string
  (** Name of the solver used for logging. *)

  val is_available : unit -> bool
  (** Check whether given solver is correctly installed. *)

  val init : unit -> unit
  (** @raise Not_available if the solver is not available. *)

  val translate : SMT.Term.t -> formula
  (** Translate formula to solver's internal representation. *)

  val solve : SMT.Term.t -> status
  (** Translate formula to solver's internal representation and check its
      satisfiability. *)

  val eval : model -> SMT.Term.t -> SMT.Term.t
  (** Evaluate term in a model. *)

  val simplify : formula -> formula

  val show_formula : formula -> string

  val to_smt_benchmark : formula -> string

  val show_model : model -> string

end

module type SMTLIB_BACKEND = sig

  val name : string

end

module type SMTLIB_BACKEND_EXTENDED = sig

  include SMTLIB_BACKEND

  val translate_non_standard : SMT.Term.t -> string

  val translate_sort_non_standard : SMT.Term.t -> string

end
