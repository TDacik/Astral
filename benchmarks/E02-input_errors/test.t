  $ astral 01-builtin_predicate_not_defined.smt2
  Parser error: Unknown application 'ls':
  Loc:line 1, character 171-179
  [3]

  $ astral 02-constructor_redefined.smt2
  Parser error: constructor 'c' redefined:
  Loc:line 1, character 206-263
  [3]

  $ astral 03-wrong_expected_status.smt2
  unsat
  [Internal error] Expected status is sat
  [1]
