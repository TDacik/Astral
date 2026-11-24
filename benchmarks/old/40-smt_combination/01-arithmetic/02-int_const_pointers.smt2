(set-info :source Astral)
(set-info :status sat)

(declare-heap (Int Int))

(declare-const v Int)

(assert
  (sep
    (pto 1 v)
    (pto 2 v)
  )
)

(check-sat)
