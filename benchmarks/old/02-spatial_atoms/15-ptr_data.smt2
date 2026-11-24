(set-info :source Astral)
(set-info :status sat)

(declare-sort RefData 0)

(declare-datatype Data
  ((c_Grid (left Int) (right Int) (down Int) (up Int)))
)

(declare-heap (RefData Data))

(declare-const x1 RefData)
(declare-const x2 RefData)
(declare-const x3 RefData)


(assert
  (sep
     (pto x1 (c_Grid 0 1 2 42))
     (pto x2 (c_Grid 0 1 2 42))
     (pto x3 (c_Grid 0 1 2 42))
  )
)

(check-sat)
