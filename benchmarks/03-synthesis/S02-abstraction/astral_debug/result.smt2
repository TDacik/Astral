;; Result of the synthesis

(set-logic SHID)

(declare-sort Ref_LS 0)

(declare-datatype LS ((c_LS (next Ref_LS))))

(declare-heap (Ref_LS LS))

(define-fun-rec pred1 ((V0 Ref_LS) (V1 Ref_LS) (V2 Ref_LS)) Bool
 (or (= V0 V2)
  (exists ((E1 Ref_LS))
   (sep (pto V0 (c_LS E1)) (pred1 E1 V1 V2) (distinct V2 V0)))))

(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const z Ref_LS)

(assert (sep (pred1 y nil z) (pred1 x z y)))

(check-sat)