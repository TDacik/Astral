; Non-eliminable existential variable n2 should not be considered
; as one of the parameters of the synthesis.

(declare-sort Ref_LS 0)
(declare-sort Ref_NLS 0)

(declare-datatype LS ((c_LS (next Ref_LS))))
(declare-datatype NLS ((c_NLS (top Ref_NLS) (next Ref_LS))))

(declare-heap (Ref_NLS NLS) (Ref_LS LS))

(define-fun-rec ls ((x Ref_LS) (y Ref_LS)) Bool
  (or
    (= x y)
    (exists ((n Ref_LS))
      (sep
        (distinct x y)
        (pto x (c_LS n))
        (ls n y)
      ))))

(declare-const x Ref_NLS)
(declare-const y Ref_NLS)

(assert (! (= x y) :positive-example))

(assert (! (exists ((n1 Ref_LS) (n2 Ref_LS))
  (sep
    (distinct x y)
    (pto x (c_NLS y n1))
    (ls n1 n2)
    (ls n2 nil)
  )
) :positive-example))

(compute-abstraction)
