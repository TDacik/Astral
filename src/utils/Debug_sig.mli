
type status := [ `Sat | `Unsat | `Unknown]

module type DEBUG_OUTPUT = sig

  val sl_formula : ?source:string -> ?status:status -> string -> SL.t -> unit

  val sl_benchmark : ?source:string -> ?status:status -> string -> ParserContext.t -> unit

  val inductive_predicate : ?name:string -> InductiveDefinition.t -> unit

  val input : ?source:string -> ?status:status -> string -> ParserContext.t -> unit

  val context : ?source:string -> ?status:status -> string -> Context.t -> unit

  val result : Context.t -> unit

  val smt_formula : ?source:string -> ?status:status -> string -> SMT.t -> unit

  val sl_model : string -> StackHeapModel.t -> unit

  val smt_model : string -> SMT.Model.t -> unit

  val output : string -> ('a -> string) -> 'a -> unit

  val output_apply : string -> ('a -> 'b) -> ('b -> string) -> 'a -> unit

end

module type EXTENDED_LOGGER = sig
  include Logger_sig.LOGGER
  include DEBUG_OUTPUT
end
