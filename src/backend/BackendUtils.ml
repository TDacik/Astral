let binary_exists name =
  match Sys.command (name ^ " --version >/dev/null 2>/dev/null") with
  | 0 -> true
  | _ -> false
