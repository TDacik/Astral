(* Utilities for performance measurement
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Unix

module P = Printer.Make (struct let name = "Timer" end)

let self = ref []

let total = ref Float.minus_one

let add name = self := ((name, (Unix.times ()).tms_utime) :: !self)

let finish () = total := (Unix.times ()).tms_utime

let json_repr () = `Assoc (List.map (fun (name, t) -> (name, `Float t)) !self)

let show () =
  List.map (fun (name, t) -> Format.asprintf "%s\t%f" name t) (List.rev !self)
  |> String.concat "\n  "

let report () =
  if Options.profile () then begin
    P.info "[Timer]\n  %s" (show ());
    P.info "\n  ======================\n";
    P.info "  Total: \t%f\n" !total
  end
