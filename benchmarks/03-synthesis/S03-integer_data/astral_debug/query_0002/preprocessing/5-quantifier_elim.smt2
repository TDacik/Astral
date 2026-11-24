(set-info :status unknown)

(declare-const x Ref_LS)
(declare-const d1 (_ BitVec 8))
(declare-const n1 Ref_LS)
(declare-const n2 Ref_LS)
(declare-const y Ref_LS)
(declare-const d2 (_ BitVec 8))
(declare-const d3 (_ BitVec 8))

(assert
 (sep
  (sep (distinct x n1 n2 y) (pto x (c_LS n1 d1)) (pto n1 (c_LS n2 d2))
   (pto n2 (c_LS y d3)))
  (distinct d2 d3)))