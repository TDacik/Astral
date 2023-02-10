(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(assert (not true))

(check-sat)

; Tests
(set-info :location_bound_min 1)
(set-info :location_bound_max 1)
