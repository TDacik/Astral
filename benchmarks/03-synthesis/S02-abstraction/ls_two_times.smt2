(declare-sort Ref_LS 0)

(declare-datatype LS ((c_LS (next Ref_LS))))

(declare-heap (Ref_LS LS))

(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const z Ref_LS)

(assert (!
  (sep
    (= x y)
    (= y z)
  )
  :positive-example)
)

(assert (! (exists ((n1 Ref_LS) (n2 Ref_LS))
  (sep
    (distinct x n1 y)
    (pto x (c_LS n1))
    (pto n1 (c_LS y))

    (distinct y n2 z)
    (pto y (c_LS n2))
    (pto n2 (c_LS z))
  )
) :positive-example))


(assert (! (exists ((n1 Ref_LS) (n2 Ref_LS) (n3 Ref_LS) (n4 Ref_LS))
  (sep
    (distinct x n1 n2 y)
    (pto x (c_LS n1))
    (pto n1 (c_LS n2))
    (pto n2 (c_LS y))

    (distinct y n3 n4 z)
    (pto y (c_LS n3))
    (pto n3 (c_LS n4))
    (pto n4 (c_LS z))
  )
) :positive-example))

(compute-abstraction)
