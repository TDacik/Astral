(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)

(assert (pto x (locPair nil nil)))

(assert (not (dls x x nil nil)))

(check-sat)
