(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(assert (not false))

(check-sat)

; Tests
(set-info :location_bound_min 1)
(set-info :location_bound_max 1)
