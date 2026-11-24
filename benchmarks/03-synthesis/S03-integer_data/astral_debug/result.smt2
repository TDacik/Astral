;; Result of the synthesis

(set-logic SHID)

(declare-sort Ref_LS 0)

(declare-datatype LS ((c_LS (next Ref_LS) (data (_ BitVec 8)))))

(declare-heap (Ref_LS LS))

(define-fun-rec pred1 ((V0 Ref_LS) (V1 Ref_LS)) Bool
 (or (= V1 V0)
  (exists ((E1 (_ BitVec 8)) (E2 Ref_LS))
   (sep (pto V0 (c_LS E2 E1)) (pred1 E2 V1) (distinct V0 V1)))))

(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert (pred1 x y))

(check-sat)