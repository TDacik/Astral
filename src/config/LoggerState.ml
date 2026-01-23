(* Internal state of logging.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

open FileUtils

(** Toplevel directory used only when Astral is used as a library. *)
let session = ref ""

(** Directory for each check-sat command passed to Astral. *)
let query = ref ""

(** Directory for each sub-query created internally by Astral. *)
let sub_query = ref ""

let query_counter = ref 0
let sub_query_counter = ref 0

let current_query () = !query_counter
let current_sub_query () = !sub_query_counter

let init () =
  UnicodeSymbols.init true; (* TODO: read from config *)
  if Config.Interactive.get () && Config.Debug.get () then
    let _ = session := Config.DebugDir.get () in
    FileUtils.mk_dir_force !session
  else if Config.Debug.get () then
    let _ = query := Config.DebugDir.get () in
    FileUtils.mk_dir_force !query
  else ()

let next_query () =
  incr query_counter;
  if Config.Debug.get () then begin
    query := Format.asprintf "query_%04d" !query_counter;
    FileUtils.mk_dir_force @@ !session ++ !query
  end

let next_sub_query () =
  incr sub_query_counter;
  if Config.Debug.get () then begin
    sub_query := Format.asprintf "query_%04d" !sub_query_counter;
    FileUtils.mk_dir_force @@ !session ++ !query ++ !sub_query
  end

let session_path ?(suffix="") filename =
  !session ++ filename ^ suffix

let query_path ?(suffix="") filename =
  !session ++ !query ++ filename ^ suffix

let sub_query_path ?(suffix="") filename =
  !session ++ !query ++ !sub_query ++ filename ^ suffix
