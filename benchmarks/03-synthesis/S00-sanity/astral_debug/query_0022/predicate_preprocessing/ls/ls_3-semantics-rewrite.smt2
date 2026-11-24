(set-info :status unknown)

(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert
 (ite (= x y) emp (sep (pto x (c_LS (next[.] x))) (ls (next[.] x) y))))