(declare-sort Ref_LS 0)
(declare-sort Ref_Data 0)

(declare-datatype Data ((c_Data (next (_ BitVec 4)))))

(declare-datatype LS ((c_LS (next Ref_LS) (data Ref_Data))))

(declare-heap (Ref_LS LS) (Ref_Data Data))

(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const d Ref_Data)

(assert (! (= x y) :positive-example))

(assert (! (exists ((n1 Ref_LS))
  (sep
    (distinct x n1 y)
    (pto x (c_LS n1 d))
    (pto n1 (c_LS y d))
  )
) :positive-example))

(assert (! (exists ((n1 Ref_LS) (n2 Ref_LS))
  (sep
    (distinct x n1 n2 y)
    (pto x (c_LS n1 d))
    (pto n1 (c_LS n2 d))
    (pto n2 (c_LS y d))
  )
) :positive-example))

(compute-abstraction)
