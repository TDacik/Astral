(set-info :source Astral)
(set-info :status sat)

(declare-sort Ref_DLS 0)

(declare-datatype DLS ((c_DLS (next Ref_DLS) (prev Ref_DLS))))

(declare-heap (Ref_DLS DLS))

; Doubly-linked list:
;   --------------------------
;   y'  <-  x  <-> ... <->  x'  ->  y
;           -------------------------

(define-fun-rec dls ((x Ref_DLS) (y Ref_DLS) (xp Ref_DLS) (yp Ref_DLS)) Bool
  (or
    (and (= x y) (= xp yp))
    (exists ((n Ref_DLS))
      (sep
        (distinct x y)
	(distinct xp yp)
	(pto x (c_DLS n yp))
	(dls n y xp x)
      )
    )
  )
)

(declare-const x Ref_DLS)
(declare-const xp Ref_DLS)

(assert
  (sep
    (distinct x xp nil)
    (dls x nil xp nil)
  )
)

(assert (not
  (sep
    (pto x  (c_DLS xp nil))
    (pto xp (c_DLS nil x))
  )
))

(check-sat)
