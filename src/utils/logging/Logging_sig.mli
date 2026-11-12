(* Signatures for logging.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022-2026 *)

(** Alias for readability *)
type filepath = string

(** Description of location where the output should be stored. *)
type location = [
  | `Session of filepath
    (** Always represents session directory. It is an error to use this
        when Astral is not in interactive mode. *)
  | `Toplevel of filepath
    (** Represents the toplevel directory. In interactive mode, this is the
        session directory, otherwise it is the query directory. *)
  | `Query of filepath
    (** The directory for the current query *)
  | `CurrentSubquery of filepath
    (** The directory for the current sub-query *)
]

(** Config for simple textual logging. *)
module type CONFIG = sig

  val name : string
  (** Name used to prefix the messages as [name] msg. *)

  val level : int
  (** Default verbosity level. *)

end

(** Config for logger with output to debug directory. *)
module type CONFIG_WITH_PATH = sig
  include CONFIG

  val path : location

end

(** {2 Output signatures} *)

module type LOGGER = sig

  val warning : ('a, Format.formatter, unit) format -> 'a

  val error : ('a, Format.formatter, unit) format -> 'a

  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a

end

type status = [`Sat | `Unsat | `Unknown]

module type LOGGER_WITH_OUTPUT = sig

  include LOGGER

  val sl_formula : ?source:string -> ?status:status -> string -> SL.t -> unit

  val sl_benchmark : ?source:string -> ?status:status -> string -> ParserContext.t -> unit

  val inductive_predicate : ?name:string -> InductiveDefinition.t -> unit

  val input : ?source:string -> ?status:status -> string -> ParserContext.t -> unit

  val context : ?source:string -> ?status:status -> string -> Context.t -> unit

  val result : Context.t -> unit

  val sl_graph : string -> SL_graph.t -> unit

  val smt_formula : ?source:string -> ?status:status -> string -> SMT.t -> unit

  val sl_model : string -> StackHeapModel.t -> unit

  val smt_model : string -> SMT.Model.t -> unit

  val output : string -> ('a -> string) -> 'a -> unit

  val output_apply : string -> ('a -> 'b) -> ('b -> string) -> 'a -> unit

  val generic : filepath -> (Stdlib.out_channel -> 'a -> unit) -> 'a -> unit

end
