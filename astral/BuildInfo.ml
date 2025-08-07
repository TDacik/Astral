module Info = Build_info.V1

let version () = match Info.version () with
  | Some v -> Info.Version.to_string v
  | None -> "not-available"

let libraries () =
  let module L = Info.Statically_linked_library in
  Info.Statically_linked_libraries.to_list ()
  |> List.map (fun lib -> Format.asprintf "%s" (L.name lib)) (*L.version lib)*)
  |> String.concat "\n  "

let print () =
  Format.printf "Astral version: %s\n" (version ());
  Format.printf "Statically linked libraries: %s\n" (libraries ())
