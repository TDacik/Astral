(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Int)

(assert (= (* x 0) 1))

(check-sat)
