(* Interface for printing and debugging
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Printer_sig

module Make (P : PRINTER) : sig

  val info : ('a, Format.formatter, unit) format -> 'a

  val warning : ('a, Format.formatter, unit) format -> 'a

  val debug : ('a, Format.formatter, unit) format -> 'a

end
