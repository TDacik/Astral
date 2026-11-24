(set-info :status unknown)

(declare-const y Ref_LS)
(declare-const x Ref_LS)
(declare-const n1 Ref_LS)

(assert
 (sep
  (exists
   ((d3 (_ BitVec 8)) (d2 (_ BitVec 8)) (d1 (_ BitVec 8)) (n2 Ref_LS)
    (n1 Ref_LS))
   (sep (distinct x n1 n2 y) (pto x (c_LS n1 d1)) (pto n1 (c_LS n2 d2))
    (pto n2 (c_LS y d3))))
  (distinct x n1)))