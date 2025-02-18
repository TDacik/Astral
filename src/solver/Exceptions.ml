(* Representation of internal exceptions used by solver.
 *
 * Author: Tomas Dacik (idacik00@fit.vut.cz), 2025 *)

exception UnknownResult of string * string

exception UnsupportedFragment of string * string

exception CmdOptionError of string

exception InternalError of string * string

let unsupported_fragment ~reason ?(details="") =
  raise @@ UnsupportedFragment (reason, details)

let unknown_result ~reason ?(details="") =
  raise @@ UnknownResult (reason, details)

let internal_error ~reason ?(details="") =
  raise @@ InternalError (reason, details)


(** Pretty printers *)
let pretty_internal_error reason details =
  Format.eprintf "%s[Internal error]%s %s\n" Colors.red Colors.white reason;
  (*if backtrace then begin
    Format.eprintf "\nBacktrace:\n%s"
      (Printexc.raw_backtrace_to_string stack)
  end;*)
  if true then Format.fprintf Format.err_formatter "\n%s\n" details
