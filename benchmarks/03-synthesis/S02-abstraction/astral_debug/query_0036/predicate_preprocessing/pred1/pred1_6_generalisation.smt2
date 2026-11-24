(set-info :status unknown)

(declare-const V0 Ref_LS)
(declare-const V1 Ref_LS)
(declare-const V2 Ref_LS)

(assert
 (ite (= V0 V2) emp
  (sep (pto V0 (c_LS (next[.] V0))) (pred1 (next[.] V0) V1 V2))))