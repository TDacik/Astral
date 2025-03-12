(set-info :source Astral)
(set-info :status sat)

(declare-sort Ref_LS 0)
(declare-sort Ref_NLS 0)

(declare-datatype LS ((c_LS (down Ref_LS))))
(declare-datatype NLS ((c_NLS (next Ref_NLS) (prev Ref_NLS) (down Ref_LS))))

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

(define-fun-rec nls ((x Ref_NLS) (y Ref_NLS) (x_rev Ref_NLS) (y_rev Ref_NLS) (z Ref_LS)) Bool
  (or
    (and (= x y) (= x_rev y_rev))
    (exists ((t Ref_NLS) (d Ref_LS))
      (sep
        (distinct x y)
	(distinct x_rev y_rev)
	(pto x (c_NLS t y_rev d))
	(nls t y x_rev x z)
	(ls d z)
      )
    )
  )
)

(declare-const x Ref_NLS)
(declare-const x_r Ref_NLS)

(assert
  (sep
    (distinct x x_r nil)

    (nls x nil x_r nil nil)
  )
)

(assert (not
  (sep
    (pto x (c_NLS x_r nil nil))
    (pto x_r (c_NLS nil x nil))
  )
))

(check-sat)
