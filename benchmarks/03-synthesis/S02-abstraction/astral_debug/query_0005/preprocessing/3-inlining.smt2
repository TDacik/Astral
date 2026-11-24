(set-info :status unknown)

(declare-const n3 Ref_LS)
(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const z Ref_LS)

(assert
 (exists ((|e!5| Ref_LS))
  (sep (pto n3 (c_LS |e!5|))
   (exists ((n4 Ref_LS) (n3 Ref_LS) (n2 Ref_LS) (n1 Ref_LS))
    (sep (distinct x n1 n2 y) (pto x (c_LS n1)) (pto n1 (c_LS n2))
     (pto n2 (c_LS y)) (distinct y n3 n4 z) (pto y (c_LS n3))
     (pto n3 (c_LS n4)) (pto n4 (c_LS z)))))))