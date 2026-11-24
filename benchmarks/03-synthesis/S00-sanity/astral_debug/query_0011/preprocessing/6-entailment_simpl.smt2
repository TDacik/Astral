(set-info :status unknown)

(declare-const x Ref_NLS)
(declare-const y Ref_NLS)
(declare-const d1 Ref_LS)
(declare-const d2 Ref_LS)
(declare-const z Ref_LS)
(declare-const n1 Ref_NLS)

(assert
 (sep
  (sep (distinct x n1 y) (distinct d1 z (as nil Ref_LS))
   (distinct d2 z (as nil Ref_LS)) (pto x (c_NLS n1 d1))
   (pto n1 (c_NLS y d2)) (pto d1 (c_LS z)) (ls d2 z))
  (distinct (as nil Ref_NLS) n1)))