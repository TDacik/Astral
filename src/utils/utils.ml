(* General utilities.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let report_msg = "Please report this issue at https://github.com/TDacik/Astral/issues."

let internal_error msg =
  Format.fprintf Format.err_formatter "[Internal error] %s\n\n%s\n" msg report_msg;
  exit 2
