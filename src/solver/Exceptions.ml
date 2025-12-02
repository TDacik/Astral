(* Representation of internal exceptions used by solver.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

exception Unsat of string

exception UnknownResult of string * string

exception UnsupportedFragment of string * string

exception InternalError of Printexc.raw_backtrace * string * string

let unsupported_fragment ~reason ?(details="") =
  raise @@ UnsupportedFragment (reason, details)

let unknown_result ~reason ?(details="") =
  raise @@ UnknownResult (reason, details)

let internal_error ~reason ?(details="") =
  let trace = Printexc.get_callstack 1000 in
  raise @@ InternalError (trace, reason, details)

(** Pretty printers *)
let pretty_internal_error ?trace reason ~details =
  Format.eprintf "%s[Internal error]%s %s\n" Colors.red Colors.white reason;
  if true then (Format.fprintf Format.err_formatter "\n%s\n" details);
  begin match trace with
    | None -> ()
    | Some trace -> Format.eprintf "\nBacktrace:\n%s" (Printexc.raw_backtrace_to_string trace)
  end
