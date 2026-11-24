(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const next Loc)

(assert (pto next next))

(check-sat)
