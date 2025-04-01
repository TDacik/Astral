(* Freed as a built-in predicate.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

include ID_sig.BUILTIN

val register : unit -> unit

val is_present : SL.t -> bool

val mk : SL.Term.t -> SL.t
