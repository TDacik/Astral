(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x Loc)

(assert (= x x))
(assert (not emp))

(check-sat)
