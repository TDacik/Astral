(set-info :source Astral)
(set-info :status sat)

; def-begin

;; Included from 00-definition.smt2 (modify there and run scripts/include_definitions.py)
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


; def-end

(declare-const x Ref_NLS)

(assert
  (sep
    (distinct x nil)
    (nls x nil nil)
  )
)

(assert (not (pto x (c_NLS nil nil))))

(check-sat)
