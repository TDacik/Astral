
let (/) path1 path2 =
  if String.equal path1 "" then path2
  else if String.equal path2 "" then path1
  else path1 ^ "/" ^ path2

(** Recursively remove a directory *)
let rec rm path =
  if Sys.is_directory path then begin
    Sys.readdir path
    |> Array.iter (fun f -> rm @@ Filename.concat path f);
    Sys.rmdir path
  end
  else Sys.remove path

let mk_dir path =
  Unix.mkdir path 0o775

let rec mk_dir_p path =
  if Sys.file_exists path then ()
  else (
    mk_dir_p @@ Filename.dirname path;
    mk_dir path
  )

(** Initialize debug model *)
let mk_dir_force path =
  if Sys.file_exists path then rm path else ();
  Sys.mkdir path 0o775

let init path =
  if not @@ Sys.file_exists path then
    match Sys.command @@ Format.asprintf "mkdir -p %s" path with
    | 0 -> ()
    | _ -> failwith ("Cannot create directory " ^ path)
