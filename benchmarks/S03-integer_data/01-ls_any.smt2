(declare-sort Ref_LS 0)

(declare-datatype LS ((c_LS (next Ref_LS) (data (_ BitVec 8)))))

(declare-heap (Ref_LS LS))

(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert (! (= x y) :positive-example))

(assert (! (exists ((n1 Ref_LS) (d1 (_ BitVec 8)) (d2 (_ BitVec 8)))
  (sep
    (distinct x n1 y)
    (pto x (c_LS n1 d1))
    (pto n1 (c_LS y d2))
  )
) :positive-example))

(assert (! (exists ((n1 Ref_LS) (n2 Ref_LS) (d1 (_ BitVec 8)) (d2 (_ BitVec 8)) (d3 (_ BitVec 8)))
  (sep
    (distinct x n1 n2 y)
    (pto x (c_LS n1 d1))
    (pto n1 (c_LS n2 d2))
    (pto n2 (c_LS y d3))
  )
) :positive-example))

(compute-abstraction)
