(* Unicode symbols for pretty printing.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

(* {3 First-order logic} *)

val eq : string ref
val neq : string ref

val and_ : string ref
val or_ : string ref
val not : string ref

val exists : string ref
val forall : string ref

val entails : string ref

val bottom : string ref

val empty_set : string ref

(* *)

val maps_to : string ref
val defined : string ref


(* {4 Separation logic.} *)

val star : string ref
val septraction : string ref

val init : bool -> unit

(**/**)

val easter_eggs : bool -> unit

val alien : string ref
