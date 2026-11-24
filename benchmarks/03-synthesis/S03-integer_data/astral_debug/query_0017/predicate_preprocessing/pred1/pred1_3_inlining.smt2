(set-info :status unknown)

(declare-const V0 Ref_LS)
(declare-const V1 Ref_LS)

(assert
 (ite (= V1 V0) emp
  (sep (pto V0 (c_LS (next[.] V0) (data[.] V0))) (pred1 (next[.] V0) V1))))