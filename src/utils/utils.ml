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

let internal_error ?(backtrace=true) ?(report=false) ?(exit_code=2) msg =
  let stack = Printexc.get_callstack 1000000 in
  Format.eprintf "%s[Internal error]%s %s\n"
    Colors.red Colors.white msg;
  if backtrace then begin
    Format.eprintf "\nBacktrace:\n%s"
      (Printexc.raw_backtrace_to_string stack)
  end;
  if report then begin
    Format.fprintf Format.err_formatter "\n%s\n" report_msg;
  end;
  exit exit_code
