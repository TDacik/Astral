(set-info :source Astral)
(set-info :status sat)

(declare-heap (Int Int))

(declare-const x Int)
(declare-const y Int)

(assert (pto x y))

(check-sat)
