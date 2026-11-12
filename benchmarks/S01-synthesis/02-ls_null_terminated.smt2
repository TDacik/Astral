(declare-sort Ref_LS 0)

(declare-datatype LS ((c_LS (next Ref_LS))))

(declare-heap (Ref_LS LS))

(declare-const x Ref_LS)

(assert (! (= x nil) :positive-example))

(assert (! (exists ((n1 Ref_LS))
  (sep
    (pto x (c_LS n1))
    (pto n1 (c_LS nil))
  )
) :positive-example))


(assert (! (exists ((n1 Ref_LS) (n2 Ref_LS))
  (sep
    (pto x (c_LS n1))
    (pto n1 (c_LS n2))
    (pto n2 (c_LS nil))
  )
) :positive-example))

(compute-abstraction)
