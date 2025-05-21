;; Doubly-linked list of singly-linked lists

(declare-sort Ref_LS 0)
(declare-sort Ref_DNLS 0)

(declare-datatype LS ((c_LS (down Ref_LS))))
(declare-datatype DNLS ((c_DNLS (next Ref_DNLS) (prev Ref_DNLS) (down Ref_LS))))

(declare-heap (Ref_LS LS) (Ref_DNLS DNLS))

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

(define-fun-rec dnls ((x Ref_DNLS) (y Ref_DNLS) (x_rev Ref_DNLS) (y_rev Ref_DNLS) (z Ref_LS)) Bool
  (or
    (sep (= x y) (= x_rev y_rev))
    (exists ((t Ref_DNLS) (d Ref_LS))
      (sep
        (distinct x y)
        (distinct x_rev y_rev)
        (pto x (c_DNLS t y_rev d))
        (dnls t y x_rev x z)
        (ls d z)
      )
    )
  )
)
