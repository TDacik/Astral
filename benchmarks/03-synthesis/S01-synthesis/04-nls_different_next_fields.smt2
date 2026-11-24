; Same as S01/07, but NLS "next" field has different name

(declare-sort Ref_LS 0)
(declare-sort Ref_NLS 0)

(declare-datatype LS ((c_LS (next Ref_LS))))
(declare-datatype NLS ((c_NLS (top Ref_NLS) (down Ref_LS))))

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

(assert (! (exists ((n1 Ref_NLS) (d1 Ref_LS) (d2 Ref_LS))
  (sep
    (distinct x n1 y)
    (distinct d1 z nil)
    (distinct d2 z nil)
    (pto x (c_NLS n1 d1))
    (pto n1 (c_NLS y d2))
    (ls d1 z)
    (ls d2 z)
  )
) :positive-example))

(compute-abstraction)
