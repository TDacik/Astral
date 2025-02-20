(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)

(declare-heap (Loc Int))

(declare-const x Int)
(declare-const y Int)

(assert (pto x y))

(check-sat)
