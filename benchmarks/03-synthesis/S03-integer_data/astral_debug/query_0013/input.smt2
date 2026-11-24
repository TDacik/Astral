(set-info :status unknown)

(declare-const y Ref_LS)
(declare-const x Ref_LS)
(declare-const n1 Ref_LS)

(assert
 (sep
  (exists ((d2 (_ BitVec 8)) (d1 (_ BitVec 8)) (n1 Ref_LS))
   (sep (distinct x n1 y) (pto x (c_LS n1 d1)) (pto n1 (c_LS y d2))))
  (distinct x n1)))