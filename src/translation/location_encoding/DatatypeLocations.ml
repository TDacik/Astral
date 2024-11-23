(* Encoding of locations using a datatype with constant constructors.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT

open SortBound

module Self = struct

  let name = "datatype"

  type internal = unit

  type t = internal Location_sig.locs (* No extra internal data needed *)

  (** {2 Initialization} *)

  let null_name = "Loc_nil"

  let null_const sort = SMT.Enumeration.mk_const sort null_name

  let const_name sl_sort index = Format.asprintf "%s_%d" (Sort.name sl_sort) index

  let const_names sl_sort bound =
    if bound = 0 then []
    else
      BatList.range 0 `To (bound - 1)
      |> BatList.map (const_name sl_sort)
      |> (@) [null_name]

  let location_encoding consts =
    Sort.Map.values consts
    |> List.flatten
    |> (@) [null_name]
    |> BatList.unique ~eq:String.equal (* Duplicate nils *)

  let sort_encoding loc_sort sl_sort const_names =
    let name = Sort.name sl_sort in
    let set_sort = Sets.mk_sort loc_sort in
    let consts = List.map (SMT.Enumeration.mk_const loc_sort) const_names in
    SMT.Sets.mk_var name set_sort, consts

  let init_constants bounds =
    LocationBounds.fold (fun sort bound acc ->
      if Sort.is_nil sort then acc
      else Sort.Map.add sort (const_names sort bound.total) acc
    ) bounds Sort.Map.empty

  let init_sort_encoding loc_sort =
    Sort.Map.mapi (fun sl_sort consts -> sort_encoding loc_sort sl_sort consts)

  let init heap_sort bounds =
    (* First, build list of constants that are needed to initialise location sort *)
    let consts = init_constants bounds in
    let constant_names = location_encoding consts in
    let loc_sort = SMT.Enumeration.mk_sort "Locations" constant_names in
    let null = null_const loc_sort in
    let constants = SMT.Enumeration.get_constants_terms loc_sort in
    let sort_encoding = init_sort_encoding loc_sort consts in
    LocationUtils.init () bounds heap_sort loc_sort null constants sort_encoding

  (** {2 Location signature} *)

  let heap_axioms self term = SMT.Boolean.tt

  let lemmas _ = Boolean.tt

  let show _ = "none" (* No internal data *)

end

include LocationBuilder.Make(Self)
