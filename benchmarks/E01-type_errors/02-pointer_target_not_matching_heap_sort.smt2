(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)

(declare-heap (Loc Int))

(declare-const x Loc)
(declare-const y Loc)

(assert (pto x y))

(check-sat)
