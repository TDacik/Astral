(set-info :status unknown)

(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const z Ref_LS)

(assert
 (exists ((|e!12| Ref_LS))
  (sep (pto z (c_LS |e!12|))
   (exists ((n2 Ref_LS) (n1 Ref_LS))
    (sep (distinct x n1 y) (pto x (c_LS n1)) (pto n1 (c_LS y))
     (distinct y n2 z) (pto y (c_LS n2)) (pto n2 (c_LS z)))))))