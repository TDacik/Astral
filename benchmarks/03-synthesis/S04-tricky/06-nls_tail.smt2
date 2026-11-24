; Each nls terminates with:
;
;   E d. last -> <d, nil> * ls(d, nil)
;
; This should be marked as the frame.

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
(declare-const last Ref_NLS)


(assert (! (exists ((d Ref_LS))
  (sep
    (= x last)
    ; Frame
    (pto last (c_NLS nil d))
    (ls d nil)
  )
) :positive-example))

(assert (! (exists ((d1 Ref_LS) (d2 Ref_LS))
  (sep
    (pto x (c_NLS last d1))
    (ls d1 nil)
    ; Frame
    (pto last (c_NLS nil d2))
    (ls d2 nil)
  )
) :positive-example))

(compute-abstraction)
