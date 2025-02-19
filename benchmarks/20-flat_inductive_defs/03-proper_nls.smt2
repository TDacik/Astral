(set-info :source Astral)
(set-info :status sat)

(declare-sort Ref_LS 0)
(declare-sort Ref_NLS 0)

(declare-datatype LS ((c_LS (down Ref_LS))))
(declare-datatype NLS ((c_NLS (next Ref_NLS) (down Ref_LS))))

(declare-heap (Ref_LS LS) (Ref_NLS NLS))

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

(define-fun-rec nls ((x Ref_NLS) (y Ref_NLS) (z Ref_LS)) Bool
  (or
    (= x y)
    (exists ((t Ref_NLS) (d Ref_LS))
      (sep
        (distinct x y)
	(pto x (c_NLS t d))
	(nls t y z)
	(ls d z)
      )
    )
  )
)

(declare-const x Ref_NLS)
(declare-const y Ref_NLS)
(declare-const z Ref_NLS)

(assert
  (sep
    (distinct x y)
    (nls x y z)
  )
)

(assert (not (pto x (c_NLS y z))))

(check-sat)
