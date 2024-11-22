(* Signature for solver's backend
 *
 * TODO: add solver object that can be used for setting command-line options etc.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

(** Type of backend's result parametrised by its internal representation of terms and models. *)
type ('term, 'model) status =
  | SMT_Sat of (SMT.Model.t * 'model) option (* SMT model, backend's internal model *)
  | SMT_Unsat of (SMT.t * 'term) list       (* Unsat core *)
  | SMT_Unknown of string                        (* Reason *)

(** Signature of basic solver parameters. *)
module type DESCRIPTION = sig

  val name : string

  val supports_smtlib_options : bool

  val supports_get_info : bool

  val supports_sets : bool

  val supports_quantifiers: bool

end

module type BACKEND = sig

  include DESCRIPTION

  type formula
  (** Internal representation of a formula. *)

  type model
  (** Internal representation of a model. *)

  val is_available : unit -> bool
  (** Check whether given solver is correctly installed. *)

  val init : unit -> unit
  (** @raise Not_available if the solver is not available. *)

  val translate : SMT.t -> formula
  (** Translate formula to solver's internal representation. *)

  val solve : Context.t -> SMT.Term.t -> bool -> string list -> (formula, model) status
  (** Translate formula to solver's internal representation and check its
      satisfiability.

      TODO: producing models can be included in options?

      *)

  val simplify : formula -> formula

  val show_formula : formula -> string

  val to_smtlib : SMT.t -> bool -> string list -> string

  val show_model : model -> string

end

module type SMTLIB_BACKEND = sig

  include DESCRIPTION

  val binary : string

  val default_options : string list

  val model_option : string

  val parser_implemented : bool

  (** Translation of non-standard terms and sorts *)

  type translate_term_cont := SMT.t -> string
  (** Type of continuation for term translation. *)

  type translate_sort_cont := Sort.t -> string
  (** Type of continuation for sort translation. *)

  val translate_non_std : translate_term_cont -> translate_sort_cont -> SMT.t -> string

  val declare_non_std_sort : Sort.t -> string

  val translate_non_std_sort : translate_sort_cont -> Sort.t -> string

end
