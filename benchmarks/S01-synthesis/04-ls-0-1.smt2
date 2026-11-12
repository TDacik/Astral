(declare-sort Ref_LS 0)
(declare-sort Ref_NLS 0)

(declare-datatype LS ((c_LS (next Ref_LS))))
(declare-datatype NLS ((c_NLS (top Ref_NLS) (next Ref_LS))))

(declare-heap (Ref_NLS NLS) (Ref_LS LS))

(declare-const x Ref_NLS)
(declare-const y Ref_NLS)

(assert (! (= x y) :positive-example))

(assert (! (exists ((n1 Ref_NLS) (d1 Ref_LS))
  (sep
    (distinct x n1 y)
    (distinct d1 nil)
    (pto x (c_NLS n1 d1))
    (pto n1 (c_NLS y nil))
    (pto d1 (c_LS nil))
  )
) :positive-example))

(compute-abstraction)
