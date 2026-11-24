(set-info :status unsat)

(declare-sort Ref_DLS 0)

(declare-datatype DLS ((c_DLS (next Ref_DLS) (prev Ref_DLS))))

(declare-heap (Ref_DLS DLS))

; Doubly-linked list:
;   --------------------------
;   y'  <-  x  <-> ... <->  x'  ->  y
;           -------------------------

(define-fun-rec dls ((x Ref_DLS) (y Ref_DLS) (xp Ref_DLS) (yp Ref_DLS)) Bool
  (or
    (sep (= x y) (= xp yp))
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

(declare-const !0 Ref_DLS)
(declare-const !1 Ref_DLS)
(declare-const !2 Ref_DLS)
(declare-const A_s_5 Ref_DLS)
(declare-const s_5 Ref_DLS)


(assert
 (and
  (sep (= s_5 A_s_5) (pto !2 (c_DLS nil !1)) (pto !1 (c_DLS !2 s_5))
   (pto s_5 (c_DLS !1 !0)) (distinct s_5 nil) (distinct !1 nil)
   (distinct !2 nil))
  (not
   (exists ((rhs!0 Ref_DLS) (rhs!1 Ref_DLS) (rhs!2 Ref_DLS))
    (sep (= s_5 A_s_5) (pto rhs!2 (c_DLS nil rhs!1))
     (pto rhs!1 (c_DLS rhs!2 s_5)) (pto s_5 (c_DLS rhs!1 rhs!0))
     (distinct s_5 nil) (distinct rhs!1 nil) (distinct rhs!2 nil))))))
