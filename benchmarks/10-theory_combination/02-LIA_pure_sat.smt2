(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Int)

(assert (= (- x 1) 42))

(check-sat)
