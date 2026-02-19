(* Exception raised for config errors.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

exception ConfigError of string * string

let () =
  Printexc.register_printer (function
    | ConfigError (msg, hint) -> Some (Format.asprintf "Configuration error: %s)" msg)
    | _ -> None
  )
