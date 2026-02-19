(* Utilities for solver binary.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

let print_error fmt =
  Format.kasprintf (fun msg ->
    if Unix.isatty Unix.stderr
    then Format.eprintf "%s%s%s" Colors.red msg Colors.white
    else Format.eprintf "%s\n" msg
  ) fmt

let user_error fmt : _ =
  Format.kasprintf (fun msg ->
    (if Unix.isatty Unix.stderr
    then Format.eprintf "%s[User error]%s %s" Colors.red Colors.white msg
    else Format.eprintf "%s\n" msg);
    exit 2
  ) fmt

let cmdline_error ?(hint="") msg : _ =
  let hint = match hint with
    | "" -> ""
    | hint -> Format.asprintf "\n\nTip: %s\n" hint
  in
  if Unix.isatty Unix.stderr
  then Format.eprintf "%s[Command line error]%s %s%s" Colors.red Colors.white msg hint
  else Format.eprintf "[Command line error]%s%s" msg hint;
  exit 2

let internal_error ?(backtrace=true) ~exit_code msg =
  let stack = Printexc.get_callstack 1000000 in
  Format.eprintf "%s[Internal error]%s %s\n"
    Colors.red Colors.white msg;
  if backtrace then begin
    Format.eprintf "\nBacktrace:\n%s"
      (Printexc.raw_backtrace_to_string stack)
  end;
  exit exit_code
