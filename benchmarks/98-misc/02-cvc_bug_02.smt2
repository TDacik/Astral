; Bug reported in https://github.com/cvc5/cvc5/issues/8841

(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x1 Loc)
(declare-const x2 Loc)

(assert (pto x1 x2))
(assert (not (pto x2 x2)))

(check-sat)
