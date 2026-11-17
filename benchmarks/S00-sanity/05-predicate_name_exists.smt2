; Astral used to name predicates "pred1", "pred2" etc. Synthesis should not fail
; when "pred1" is already given in the input.

(declare-sort Ref_LS 0)
(declare-sort Ref_NLS 0)

(declare-datatype LS ((c_LS (next Ref_LS))))
(declare-datatype NLS ((c_NLS (top Ref_NLS) (next Ref_LS))))

(declare-heap (Ref_NLS NLS) (Ref_LS LS))

(define-fun-rec pred1 ((x Ref_LS) (y Ref_LS)) Bool
  (or
    (= x y)
    (exists ((n Ref_LS))
      (sep
        (distinct x y)
        (pto x (c_LS n))
        (pred1 n y)
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
    (pred1 d1 z)
    (pred1 d2 z)
  )
) :positive-example))

(compute-abstraction)
