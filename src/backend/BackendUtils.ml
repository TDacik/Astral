let binary_exists name =
  match Sys.command (name ^ " --version >/dev/null 2>/dev/null") with
  | 0 -> true
  | _ -> false

let var_unique_name var =
  let name, sort = SMT.Variable.describe var in
  Format.asprintf "id%d_%s_%s" (String.length name) name (Sort.show_kind sort)
