(* Logger state.
 *
 * TODO: rework the whole logging.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

module Options = Options_base

(** Interactive mode *)

(** Reference to base debug directory for interactive mode. *)
let base_debug_dir = ref ""

let query_counter = ref 0

let next_query () =
  let next = BatRef.post_incr query_counter in
  Options.set_debug_dir (Format.asprintf "%s/query_%04d" !base_debug_dir next)

let debug_dir () =
  if Options.interactive () then
    Format.asprintf "%s/query_%04d" !base_debug_dir !query_counter
  else (Options.debug_dir ())

let init () =
  base_debug_dir := Options.debug_dir ()
