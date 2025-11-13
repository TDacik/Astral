(* Interface for printing and debugging
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

open Logger_sig

let log channel ~color ~tag fmt =
  let prefix = match tag with
    | "" -> ""
    | _ -> "[" ^ tag ^ "] "
  in
  Format.kasprintf (fun msg ->
    if Unix.isatty channel
    then Format.eprintf "%s%s%s%s\n%!" color prefix msg Colors.white
    else Format.eprintf "%s%s%s\n%!" color msg Colors.white
  ) fmt


module Make (C : CONFIG) = struct

  let match_key () =
    let key =
      Config.DebugKey.get ()
      |> (fun str -> BatString.nreplace ~sub:"|" ~by:"\\|" ~str)
      |> BatString.lowercase_ascii
      |> Format.asprintf ".*%s.*"
    in
    let regex = Str.regexp key in
    Str.string_match regex (BatString.lowercase_ascii C.name) 0

  let debug_aux () = Config.Debug.get () || Config.Verbosity.get () >= C.level

  let debug () = debug_aux () && match_key ()

  let info = Format.fprintf

  let init path =
    if not @@ Sys.file_exists path then
      match Sys.command @@ Format.asprintf "mkdir -p %s" path with
      | 0 -> ()
      | _ -> failwith ("Cannot create directory " ^ path)

  let warning fmt = log Unix.stderr ~color:Colors.yellow ~tag:C.name fmt

  let error fmt = log Unix.stderr ~color:Colors.red ~tag:C.name fmt

  let dump dump_fn filename obj =
    if Config.Debug.get () then
      let path = LoggerState.query_path filename in
      let channel = open_out path in
      dump_fn path obj;
      close_out channel

  let debug fmt =
    Format.kasprintf (fun msg ->
      if debug () && not @@ Config.Interactive.get ()
      then Format.printf "[%s] %s%!" C.name msg
    ) fmt;

end
