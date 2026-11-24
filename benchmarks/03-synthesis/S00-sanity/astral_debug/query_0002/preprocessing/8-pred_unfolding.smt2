(set-info :status unknown)

(declare-const x Ref_NLS)
(declare-const n1 Ref_NLS)
(declare-const y Ref_NLS)
(declare-const d2 Ref_LS)
(declare-const d1 Ref_LS)
(declare-const z Ref_LS)

(assert
 (sep
  (sep (distinct x n1 y) (distinct d1 z (as nil Ref_LS))
   (distinct d2 z (as nil Ref_LS)) (pto x (c_NLS n1 d1))
   (pto n1 (c_NLS y d2)) (pto d1 (c_LS z))
   (ite (= d2 z) emp (sep (pto d2 (c_LS (next[.] d2))) (= (next[.] d2) z))))
  (distinct d1 z)))