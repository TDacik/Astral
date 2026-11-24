(set-info :status unknown)

(declare-const x Ref_LS)
(declare-const d1 (_ BitVec 8))
(declare-const d2 (_ BitVec 8))
(declare-const n1 Ref_LS)
(declare-const y Ref_LS)

(assert
 (sep (sep (distinct x n1 y) (pto x (c_LS n1 d1)) (pto n1 (c_LS y d2)))
  (distinct n1 y)))