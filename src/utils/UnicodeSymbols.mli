(* Unicode symbols for pretty printing.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

(* {3 First-order logic} *)

val eq : unit -> string
val neq : unit -> string

val logand : unit -> string
val logor : unit -> string
val lognot : unit -> string
val gneg : unit -> string

val exists : unit -> string
val forall : unit -> string

val entails : unit -> string

(* {4 Separation logic.} *)

val pto : unit -> string
val star : unit -> string
val septraction : unit -> string
