(set-info :source Astral)
(set-info :status sat)

(declare-sort RefGrid 0)

(declare-datatype Grid
  ((c_Grid (left RefGrid) (right RefGrid) (down RefGrid) (up RefGrid)))
)

(declare-heap (RefGrid Grid))

(declare-const x00 RefGrid)
(declare-const x01 RefGrid)
(declare-const x02 RefGrid)

(declare-const x10 RefGrid)
(declare-const x11 RefGrid)
(declare-const x12 RefGrid)

(declare-const x20 RefGrid)
(declare-const x21 RefGrid)
(declare-const x22 RefGrid)


(assert
  (sep
     (pto x00 (c_Grid x00 x01 x10 x00))
     (pto x01 (c_Grid x00 x02 x11 x01))
     (pto x02 (c_Grid x01 x02 x12 x02))

     (pto x10 (c_Grid x10 x11 x20 x00))
     (pto x11 (c_Grid x10 x12 x21 x01))
     (pto x12 (c_Grid x11 x12 x22 x02))

     (pto x20 (c_Grid x20 x21 x20 x10))
     (pto x21 (c_Grid x20 x22 x21 x11))
     (pto x22 (c_Grid x21 x22 x22 x12))

     (distinct x00 x01 x02 x10 x11 x12 x20 x21 x22 nil)
  )
)

(check-sat)
