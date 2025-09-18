include Sort.MonoMap(SortBound)

let init_sort sort alloc total =
  add sort (SortBound.init alloc total) empty

let plus_n sort n map =
  let curr =
    try find sort map
    with Not_found -> SortBound.zero
  in
  add sort (SortBound.plus curr @@ SortBound.n n) map

let plus = union (fun sort n1 n2 -> Option.some @@ SortBound.plus n1 n2)

let find sort map =
  try find sort map
  with Not_found -> SortBound.zero
