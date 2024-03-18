(* Utilities for convertors.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

(** Rewrite variables using indices. *)
let to_index_vars ?(prefix = "") ?(start = 0) phi =
  let vars = SSL.get_vars ~with_nil:false phi in
  let worklist =
    List.mapi (fun i (name, _) ->
      (Identifier.show name, Format.asprintf "%s%d" prefix (i + start))
    ) vars
  in
  List.fold_left (fun phi (var, new_name) ->
    SSL.rename_var phi var new_name
  ) phi worklist
