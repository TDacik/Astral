(* Encoding of nested singly-linked lists.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open Context_sig
open PredicateEncoding_sig

module Default (C : CONTEXT) : PREDICATE_ENCODING with module Context = C
