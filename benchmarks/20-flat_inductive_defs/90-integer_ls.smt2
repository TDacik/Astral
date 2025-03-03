(set-info :source Astral)
(set-info :status sat)

(declare-sort Ref_LS 0)

(declare-datatype LS ((c_LS (next Ref_LS) (data Int))))

(declare-heap (Ref_LS LS))

(define-fun-rec ls ((x Ref_LS) (y Ref_LS)) Bool
  (or
    (= x y)
    (exists ((n Ref_LS) (d Int))
      (sep
        (distinct x y)
        (pto x (c_LS n d))
	(ls n y)
      )
    )
  )
)

(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert
  (sep
    (distinct x y)
    (ls x y)
  )
)

(assert (not
  (exists ((d Int))
    (pto x (c_LS y d))
  )
))

(check-sat)
