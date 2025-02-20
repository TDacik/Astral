  $ astral 01-pointer_source_is_not_location.smt2
  Parser error: pointer target has sort Int (expected a location sort):
  Loc:line 1, character 156-157
  [3]

  $ astral 02-pointer_target_not_matching_heap_sort.smt2
  Parser error: pointer target has sort (Loc Loc) (expected Int):
  Loc:line 1, character 158-159
  [3]
