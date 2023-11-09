(* Graph representation of formulae ASTs.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Logic_sig

module Make (AST : AST_BASE) : AST with type Term.t := AST.Term.t
