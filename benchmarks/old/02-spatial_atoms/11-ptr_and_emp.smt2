(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert (pto x y))
(assert sep.emp)

(check-sat)
