(* Interface for printing and debugging
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Printer_sig

module Make (P : PRINTER) = struct

  let info msg = Printf.printf msg

  let warning msg = Printf.printf msg

  let debug msg =
    if Options.debug () then Printf.printf msg
    else Printf.ifprintf stdout msg
end
