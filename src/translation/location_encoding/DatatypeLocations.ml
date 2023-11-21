(* Encoding of locations using a datatype with constant constructors.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT
open Bounds

open LocationBounds.SortBound

module Self = struct

  let name = "datatype"

  type sort_encoding = SMT.Term.t * SMT.Term.t list

  type t = {
    loc_sort : Sort.t;                         (* SMT sort of locations. *)
    sort_encodings : sort_encoding Sort.Map.t; (* Encodings of SL sorts. *)
  }

  (** {2 Initialization} *)

  let nil_name = "Loc_nil"

  let nil_const self = SMT.Enumeration.mk_const self.loc_sort nil_name

  let const_name sl_sort index = Format.asprintf "%s_%d" (Sort.show sl_sort) index

  let const_names sl_sort bound =
    if bound = 0 then []
    else
      BatList.range 0 `To (bound - 1)
      |> BatList.map (const_name sl_sort)
      |> (@) [nil_name]

  let location_encoding consts =
    Sort.Map.values consts
    |> List.flatten
    |> (@) [nil_name]
    |> BatList.unique ~eq:String.equal (* Duplicate nils *)
    |> SMT.Enumeration.mk_sort "Locations"

  let sort_encoding loc_sort sl_sort const_names =
    let name = Sort.show sl_sort in
    let set_sort = Set.mk_sort loc_sort in
    let consts = List.map (SMT.Enumeration.mk_const loc_sort) const_names in
    SMT.Set.mk_var name set_sort, consts

  let init bounds =
    (* First, build list of constants that are needed to initialise location sort *)
    let consts =
      LocationBounds.fold
        (fun sort bound acc ->
          if Sort.is_nil sort then acc (* nil sort does not have its encoding in SMT *)
          else Sort.Map.add sort (const_names sort bound.total) acc
        ) bounds.location_bounds Sort.Map.empty
    in
    let loc_sort = location_encoding consts in
    let sort_encodings =
      Sort.Map.fold
        (fun sl_sort const_names acc ->
          Sort.Map.add sl_sort (sort_encoding loc_sort sl_sort const_names) acc
        ) consts Sort.Map.empty
    in
    {
      loc_sort = loc_sort;
      sort_encodings = sort_encodings;
    }


  (** {2 Location signature} *)

  let get_sort self = self.loc_sort

  let get_sort_encoding self sl_sort = fst @@ Sort.Map.find sl_sort self.sort_encodings

  let get_constants self = Enumeration.get_constants self.loc_sort

  let get_constants_s self sl_sort = snd @@ Sort.Map.find sl_sort self.sort_encodings

  let get_index self loc =
    let consts = get_constants self in
    Option.get @@ List_utils.index_of_cmp SMT.compare loc consts

  let inverse_translate self model loc =
    let name = SMT.Term.show loc in
    let index = get_index self loc in
    if BatString.exists name "DLS_t" then
      StackHeapModel.Location.mk_dls index
    else if BatString.exists name "NLS_t" then
      StackHeapModel.Location.mk_nls index
    else if BatString.exists name "LS_t" then
      StackHeapModel.Location.mk_ls index
    else
      StackHeapModel.Location.mk_nil index

  let heap_axioms self term = SMT.Boolean.mk_true ()

  let lemmas _ = Boolean.mk_true ()

  let internal_repr self =
    let show (term, consts) =
      Format.asprintf "(%s, {%s})"
        (SMT.Term.show term)
        (String.concat ", " @@ List.map SMT.Term.show consts)
    in
    Sort.Map.show show self.sort_encodings

end

include LocationBuilder.Make(Self)
