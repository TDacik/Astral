(* Interface for printing and debugging
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

open Logger_sig

let warning fmt =
  Format.kasprintf (fun msg ->
    if Unix.isatty Unix.stderr
    then Format.eprintf "%s%s%s" Colors.yellow msg Colors.white
    else Format.eprintf "%s\n" msg
  ) fmt

let error = warning

module Make (C : CONFIG) = struct

  let match_key () =
    let key =
      Options_base.debug_key ()
      |> (fun str -> BatString.nreplace ~sub:"|" ~by:"\\|" ~str)
      |> BatString.lowercase_ascii
      |> Format.asprintf ".*%s.*"
    in
    let regex = Str.regexp key in
    Str.string_match regex (BatString.lowercase_ascii C.name) 0

  let debug_aux () = Options_base.debug () || Options_base.verbosity () >= C.level

  let debug () = debug_aux () && match_key ()

  let info = Format.fprintf

  let init path =
    if not @@ Sys.file_exists path then
      match Sys.command @@ Format.asprintf "mkdir -p %s" path with
      | 0 -> ()
      | _ -> failwith ("Cannot create directory " ^ path)

  let warning fmt =
    Format.kasprintf (fun msg ->
      if Unix.isatty Unix.stderr
      then Format.eprintf "\x1b[33m[%s] %s\x1b[0m\n%!" C.name msg
      else Format.eprintf "[%s] %s\n%!" C.name msg
    ) fmt

  (* TODO *)
  let error = warning

  let dump dump_fn filename obj =
    if Options_base.debug () then
      let dir_path = Options_base.debug_dir () in
      init dir_path;
      let path = dir_path ^ "/" ^ filename in
      let channel = open_out path in
      dump_fn path obj;
      close_out channel

  let debug fmt =
    Format.kasprintf (fun msg ->
      if debug () && not @@ Options_base.interactive ()
      then Format.printf "[%s] %s%!" C.name msg
    ) fmt;


end

module MakeWithDir (C : CONFIG_WITH_DIR) = struct
  include Make(C)

  let init path =
    if not @@ Sys.file_exists path then
      match Sys.command @@ Format.asprintf "mkdir -p %s" path with
      | 0 -> ()
      | _ -> failwith ("Cannot create directory " ^ path)


  let dump dump_fn filename obj =
    if Options_base.debug () then
      let dir_path = Options_base.debug_dir () ^ "/" ^ C.dirname in
      init dir_path;
      let path = dir_path ^ "/" ^ filename in
      let channel = open_out path in
      dump_fn path obj;
      close_out channel

  let dump_string ~filename str =
    if Options_base.debug () then
      let dir_path = Options_base.debug_dir () ^ "/" ^ C.dirname in
      init dir_path;
      let path = dir_path ^ "/" ^ filename in
      let channel = open_out path in
      Printf.fprintf channel "%s" str;
      close_out channel

end

module MakeWithDump (C : CONFIG_WITH_DUMP) = struct
  include MakeWithDir(C)

  type t = C.t

  let dump ~filename x =
    if Options_base.debug () then
      let dir_path = Options_base.debug_dir () ^ "/" ^ C.dirname in
      init dir_path;
      let path = dir_path ^ "/" ^ filename in
      C.dump x path
end
