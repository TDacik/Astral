(* General utilities.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let warning fmt =
  Format.kasprintf (fun msg ->
    if Unix.isatty Unix.stderr
    then Format.eprintf "%s%s%s" Colors.yellow msg Colors.white
    else Format.eprintf "%s\n" msg
  ) fmt
