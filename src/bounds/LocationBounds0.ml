include Sort.MonoMap(SortBound)

let plus sort n map =
  let curr = find sort map in
  add sort (SortBound.plus curr @@ SortBound.n n) map
