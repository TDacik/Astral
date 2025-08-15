  $ astral 01-wrong_expected_status.smt2
  unsat
  [Internal error] Expected status is sat
  [1]

  $ astral 02-nil_as_var.smt2
  Parser error: The name 'nil' is reserved for separation logic constant
  [3]

  $ astral 03-var_not_declared.smt2
  Parser error: variable 'x' not declared
  Loc:line 1, character 100-101
  [3]

  $ astral 04-var_redeclared.smt2
  Parser error: variable 'x' redefined
  [3]

  $ astral 05-sort_not_declared.smt2
  Parser error: sort 'Loc' not declared
  [3]

  $ astral 06-constructor_reused.smt2
  Parser error: constructor 'c' redefined
  Loc:line 1, character 206-263
  [3]

  $ astral 07-builtin_predicate_not_enabled.smt2
  Parser error: Unknown application 'ls'
  Loc:line 1, character 171-179
  [3]
