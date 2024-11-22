(* Interface for printing and debugging
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2022 *)

open Logger_sig

(** ANSI colors *)

let warning fmt =
  Format.kasprintf (fun msg ->
    if Unix.isatty Unix.stderr
    then Format.eprintf "%s%s%s" Colors.yellow msg Colors.white
    else Format.eprintf "%s\n" msg
  ) fmt

module Make (C : CONFIG) = struct

  let match_key () =
    let regex = Str.regexp @@ Options_base.debug_key () in
    Str.string_match regex C.name 0

  let debug_aux () = Options_base.debug () || Options_base.verbosity () >= C.level

  let debug () = debug_aux () && match_key ()

  let info = Format.fprintf

  let warning fmt =
    Format.kasprintf (fun msg ->
      if Unix.isatty Unix.stderr
      then Format.eprintf "\x1b[33m[%s] %s\x1b[0m\n%!" C.name msg
      else Format.eprintf "[%s] %s\n%!" C.name msg
    ) fmt

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
      Sys.mkdir path 0o775

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
