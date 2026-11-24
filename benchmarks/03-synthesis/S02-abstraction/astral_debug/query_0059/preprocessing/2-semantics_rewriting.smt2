(set-info :status unknown)

(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const z Ref_LS)

(assert (sep (= x y) (= y z)))

(assert (not (pred1 y nil z)))