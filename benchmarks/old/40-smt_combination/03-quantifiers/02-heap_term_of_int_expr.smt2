(set-info :source Astral)
(set-info :status unsat)

(declare-heap (Int Int))

(declare-const x Int)
(declare-const y Int)

(assert (pto (+ x 2) y))

(assert (not
  (exists ((v Int))
    (pto (+ x 2) v)
)))

(check-sat)
