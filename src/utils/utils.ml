(* General utilities.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let report_msg = "Please report this issue at https://github.com/TDacik/Astral/issues."

let error msg =
  Format.fprintf Format.err_formatter "%s\n" msg;
  exit 1

let cmd_option_error opt value =
  Format.fprintf Format.err_formatter "Unknown %s: '%s'\n" opt value;
  exit 1

let internal_error ?(backtrace=true) msg =
  let stack = Printexc.get_callstack 1000000 in
  Format.fprintf Format.err_formatter "[Internal error] %s\n\n" msg;
  if backtrace then begin
    Format.fprintf Format.err_formatter "Backtrace:\n%s\n\n"
      (Printexc.raw_backtrace_to_string stack)
  end;
  Format.fprintf Format.err_formatter "%s\n" report_msg;
  exit 2
