(declare-sort Ref_LS 0)

(declare-datatype LS ((c_LS (next Ref_LS))))

(declare-heap (Ref_LS LS))

(define-fun-rec ls ((x Ref_LS) (y Ref_LS)) Bool
  (or
    (= x y)
    (exists ((n Ref_LS))
      (sep
        (distinct x y)
        (pto x (c_LS n))
        (ls n y)
      ))))

(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert (! (exists ((n1 Ref_LS) (n2 Ref_LS) (n3 Ref_LS))
  (sep
    (distinct x y)
    (distinct n1 y)
    (distinct n2 y)
    (distinct n3 y)
    (pto x (c_LS n1))
    (pto n1 (c_LS n2))
    (pto n2 (c_LS n3))
    (pto n3 (c_LS y))
  )
) :positive-example))

(compute-abstraction)
