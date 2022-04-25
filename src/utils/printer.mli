(* Interface for printing and debugging
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Printer_sig

module Make (P : PRINTER) : sig

  val info : ('a, out_channel, unit) format -> 'a

  val warning : ('a, out_channel, unit) format -> 'a

  val debug : ('a, out_channel, unit) format -> 'a

end
