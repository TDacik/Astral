(* Interface for printing and debugging
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Printer_sig

module Make (P : PRINTER) = struct

  let info msg = Format.printf msg

  let warning msg = Format.printf msg

  let debug msg =
    if Options_base.debug () && not @@ Options_base.interactive () then Format.printf msg
    else Format.ifprintf Format.err_formatter msg
end
