(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)
(declare-const z1 Loc)

(assert (ls x y))

(assert (sep (not emp) (not emp) (not emp)))

(check-sat)
