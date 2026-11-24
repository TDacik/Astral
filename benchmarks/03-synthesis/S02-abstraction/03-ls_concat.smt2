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

(assert (! (exists ((n1 Ref_LS))
  (sep
    (ls x n1)
    (ls n1 nil)
  )
) :positive-example))

(compute-abstraction)
