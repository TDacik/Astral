(set-info :source Astral)
(set-info :status unsat)

(declare-heap (Int Int))

(declare-const x Int)
(declare-const y Int)

(assert (pto x y))

(assert (not
  (exists ((v Int))
    (pto x v)
)))

(check-sat)
