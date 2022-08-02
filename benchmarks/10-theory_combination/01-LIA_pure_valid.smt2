(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(assert (= (+ 1 1) 2))

(check-sat)
