(set-info :status unknown)

(declare-const V1 Ref_LS)
(declare-const V2 Ref_LS)
(declare-const V0 Ref_LS)

(assert
 (or (= V0 V2)
  (exists ((E1 Ref_LS))
   (sep (pto V0 (c_LS E1)) (pred1 E1 V1 V2) (distinct V2 V0)))))