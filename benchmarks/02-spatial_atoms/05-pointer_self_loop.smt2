(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)

(assert (pto x x))

(check-sat)
