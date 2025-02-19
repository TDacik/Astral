  $ astral 01-nil_as_var.smt2
  Parser error: The name 'nil' is reserved for separation logic constant
  [3]

  $ astral 02-var_not_declared.smt2
  Parser error: variable 'x' not declared:
  Loc:line 1, character 100-101
  [3]

  $ astral 03-var_redeclared.smt2
  Parser error: variable 'x' redefined
  [3]

  $ astral 04-sort_not_declared.smt2
  Parser error: sort 'Loc' not declared
  [3]

TODO
$ astral 05-pointer_not_matching_heap_sort.smt2
...
[3]
