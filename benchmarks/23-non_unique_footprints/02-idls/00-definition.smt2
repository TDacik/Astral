;; Imprecise doubly-linked list

(declare-sort Ref_DLS 0)

(declare-datatype DLS ((c_DLS (next Ref_DLS) (prev Ref_DLS))))

(declare-heap (Ref_DLS DLS))

; Doubly-linked list:
;           -------------------------
;   y'  <-  x  <-> ... <->  x'  ->  y
;   --------------------------

(define-fun-rec idls ((x Ref_DLS) (y Ref_DLS) (xp Ref_DLS) (yp Ref_DLS)) Bool
  (or
    (sep (= x y) (= xp yp))
    (exists ((n Ref_DLS))
      (sep
        (pto x (c_DLS n yp))
        (idls n y xp x)
      )
    )
  )
)
