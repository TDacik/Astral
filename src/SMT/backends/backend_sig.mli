(* Signature for solver's backend
 *
 * TODO: add solver object that can be used for setting command-line options etc.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

(** Type of backend's result parametrised by its internal representation of formula
    and models. *)
type ('term, 'model) status =
  | SMT_Sat of (SMT.Model.t * 'model) option (* SMT model, backend's internal model *)
  | SMT_Unsat of (SMT.Term.t * 'term) list   (* Unsat core *)
  | SMT_Unknown of string                    (* Reason *)

module type BACKEND = sig

  type formula
  (** Internal representation of a formula. *)

  type model
  (** Internal representation of a model. *)

  val name : string
  (** Name of the solver used for logging. *)

  val is_available : unit -> bool
  (** Check whether given solver is correctly installed. *)

  val init : unit -> unit
  (** @raise Not_available if the solver is not available. *)

  val translate : SMT.Term.t -> formula
  (** Translate formula to solver's internal representation. *)

  val solve : SMT.Term.t -> bool -> string list -> (formula, model) status
  (** Translate formula to solver's internal representation and check its
      satisfiability.

      TODO: producing models can be included in options?

      *)

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
