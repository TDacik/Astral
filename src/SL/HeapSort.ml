(* Heap sort.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open MemoryModel

module M = Sort.MonoMap(StructDef)
include M

type t = M.t

let show self =
  M.bindings self
  |> List.map (fun (dom, range) ->
      Format.asprintf "%s -> %s" (Sort.show dom) (StructDef.show range))
  |> String.concat ", "
  |> (fun s -> "(" ^ s ^ ")")

let union =
  List.fold_left (M.union (fun _ _ _ -> failwith "Heap sorts are not disjoint")) empty

let find_target sort self =
  try M.find sort self
  with Not_found -> Utils.internal_error @@ Format.asprintf
    "No target for %s in heap sort:\n\t%s" (Sort.show sort) (show self)

let find_target_unwrapped sort self =
  let target = find_target sort self in
  assert (List.length target.fields = 1);
  Field.get_sort @@ List.hd target.fields

let get_fields self =
  M.values self
  |> BatList.concat_map StructDef.get_fields
  |> BatList.unique ~eq:Field.equal

let is_loc_sort self sort =
  M.mem sort self

let get_loc_sorts self = M.keys self

let is_bitvector_model self =
  if M.cardinal self != 1 then false
  else
    let dom, range = M.choose self in
    if List.length range.fields != 1 then false
    else
      let range_sort = Field.get_sort @@ List.hd range.fields in
      Sort.is_bitvector dom && Sort.is_bitvector range_sort

module Self = struct
  type nonrec t = t
  let show = show
end

include Datatype.Printable(Self)
