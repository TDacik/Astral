(declare-sort Ref_DLS 0)

(declare-datatype DLS ((c_DLS (next Ref_DLS) (prev Ref_DLS))))

(declare-heap (Ref_DLS DLS))

(declare-const x Ref_DLS)
(declare-const y Ref_DLS)
(declare-const xp Ref_DLS)
(declare-const yp Ref_DLS)

(assert (! (sep (= x y) (= xp yp)) :positive-example))

(assert (! (exists ((n1 Ref_DLS))
  (sep
    (distinct x n1 xp y yp)
    (pto x  (c_DLS n1 yp))
    (pto n1 (c_DLS xp x))
    (pto xp (c_DLS y n1))
  )
) :positive-example))

(compute-abstraction)
