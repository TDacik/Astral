(* Identifiers.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

(*
let do_debug = ref false
let debug = Format.kasprintf (fun (msg : string) -> if !do_debug then Format.printf "%s%!" msg)
*)

type t = Int.t * String.t
(** Identifier is represented by integer tag used for comparison and name used for printing. *)

let compare (tag1, _) (tag2, _) = Int.compare tag1 tag2
let equal id1 id2 = compare id1 id2 = 0
let show (tag, name) = name
let show_debug (tag, name) = Format.sprintf "%d:%s" tag name
let tag (id, _) = id
let pp fmt (_, name) = Format.fprintf fmt "%s" name

let equal_with_string (_, name) str = String.equal name str

let base_name_str name =
  match String.split_on_char '!' name with
    | [name] | [name; _] -> name
    | _ -> assert false

let base_name (_, name) = base_name_str name

module Self = struct
  type nonrec t = t
  let show = show
  let compare = compare
end

include Datatype.Printable(Self)
include Datatype.Collections(Self)

(** Identifier managment *)

module HT = Hashtbl.Make(String)

module Make () = struct
  let counter = ref 0

  let tag_table = ref (HT.create 97)
  let index_table = ref (HT.create 97)

  let add_tag = HT.add !tag_table
  let update_index = HT.replace !index_table

  let find_tag = HT.find !tag_table
  let find_index = HT.find !index_table

  let debug_repr () =
    let debug name n acc = Format.asprintf "%s\n  %s -> %d" acc name n in
    let t1 = HT.fold debug !tag_table "Tag table" in
    let t2 = HT.fold debug !index_table "Index table" in
    t1 ^ "\n" ^ t2

  type nonrec t = t

  let next_id () = incr counter; !counter

  let mk name =
    (*debug "Creating identifier %s\n" name;*)

    let res =
      try (find_tag name, name)
      with Not_found ->
        let tag = next_id () in
        add_tag name tag;
        (tag, name)
    in
    (*debug "%s\n" (debug_repr ());*)
    res

  let mk_fresh name =
    (*debug "Creating fresh identifier %s\n" name;*)
    let name = if name = "" then "e" else name in
    let base_name = base_name_str name in
    let index =
      try find_index base_name + 1
      with Not_found -> 1
    in

    let fresh_name = Format.asprintf "%s!%d" base_name index in
    assert (not @@ HT.mem !tag_table fresh_name);

    let tag = next_id () in
    add_tag fresh_name tag;
    update_index base_name index;

    (*debug "%s\n" (debug_repr ());*)
    (tag, fresh_name)

  let show = show
  let show_debug = show_debug
  let tag = tag
  let compare = compare
  let equal = equal
  let base_name = base_name
end
