open Location_sig

let init_mapping heap_sort bounds loc_sort =
  LocationBounds.fold (fun sort bound acc ->
    if Sort.is_loc sort then acc
    else
      let to_loc = SMT.Array.mk_var "to_loc" @@ SMT.Array.mk_sort sort loc_sort in
      let of_loc = SMT.Array.mk_var "of_loc" @@ SMT.Array.mk_sort loc_sort sort in
      Sort.Map.add sort (to_loc, of_loc) acc
  ) bounds Sort.Map.empty

let init internal bounds heap_sort loc_sort null constants encoding = {
  internal = internal;
  bounds = bounds;
  heap_sort = heap_sort;
  null = null;
  sort = loc_sort;
  constants = constants;
  sort_encoding = encoding;
  mapping = init_mapping heap_sort bounds loc_sort;
}
