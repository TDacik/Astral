(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

(assert (ls x y))

(assert (sep (not emp) (not emp) (not emp)))

(check-sat)
