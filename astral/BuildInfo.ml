(* Build info.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

module Info = Build_info.V1

let version () = match Info.version () with
  | Some v -> Info.Version.to_string v
  | None -> "not-available"
