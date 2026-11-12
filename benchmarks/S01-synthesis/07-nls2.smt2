(declare-sort Ref_LS 0)
(declare-sort Ref_NLS 0)

(declare-datatype LS ((c_LS (next Ref_LS))))
(declare-datatype NLS ((c_NLS (top Ref_NLS) (next1 Ref_LS) (next2 Ref_LS))))

(declare-heap (Ref_NLS NLS) (Ref_LS LS))

(define-fun-rec ls ((x Ref_LS) (y Ref_LS)) Bool
  (or
    (= x y)
    (exists ((n Ref_LS))
      (sep
        (distinct x y)
        (pto x (c_LS n))
        (ls n y)
      )
    )
  )
)

(declare-const x Ref_NLS)
(declare-const y Ref_NLS)
(declare-const z Ref_LS)

(assert (! (= x y) :positive-example))

(assert (! (exists ((n1 Ref_NLS) (d11 Ref_LS) (d12 Ref_LS) (d21 Ref_LS) (d22 Ref_LS))
  (sep
    (distinct x n1 y)
    (distinct d11 d12 z nil)
    (distinct d21 d22 z nil)
    (pto x (c_NLS n1 d11 d12))
    (pto n1 (c_NLS y d21 d22))
    (ls d11 z)
    (ls d12 z)
    (ls d21 z)
    (ls d22 z)
  )
) :positive-example))

(compute-abstraction)
