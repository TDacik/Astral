(set-info :source Astral)
(set-info :status unsat)

(declare-heap (Int Int))

(declare-const x Int)
(declare-const y Int)

(assert
  (sep
    (pto (+ x 2) y)
    (pto (+ y 1) (+ x 2))
  )
)

(assert (not
  (exists ((v Int))
    (sep
      (pto (+ x 2) v)
      (pto (+ v 1) (+ x 2))
    )
)))

(check-sat)
