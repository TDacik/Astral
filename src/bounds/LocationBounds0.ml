include Sort.MonoMap(SortBound)

let plus sort n map =
  let curr =
    try find sort map
    with Not_found -> SortBound.zero
  in
  add sort (SortBound.plus curr @@ SortBound.n n) map

let find sort map =
  try find sort map
  with Not_found -> SortBound.zero
