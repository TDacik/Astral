(* Interface for printing and debugging
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Printer_sig

module Make (P : PRINTER) = struct

  let info msg = Format.printf msg

  let warning msg = Format.printf msg

  let debug msg =
    if Options.debug () then Format.printf msg
    else Format.ifprintf Format.std_formatter msg
end
