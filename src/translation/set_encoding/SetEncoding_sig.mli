(* Signature of set encoding.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

module type SET_ENCODING = sig

  val name : string
  (** Name of the encoding used for debugging. *)

  val rewrite : SMT.t -> SMT.t
  (** Rewrite all set symbols and operations to the encoding. *)

  val rewrite_back : SMT.t -> SMT.Model.model -> SMT.Model.model
  (** Rewrite a model back from the encoding. The original formula is provided to
      reconstruct sorts of variables. *)

end
