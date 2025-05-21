; Singly-linked list

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
      )
    )
  )
)
