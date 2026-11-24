(set-info :status unknown)

(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert
 (or (= x y)
  (sep (distinct x y) (pto x (c_LS (next[.] x))) (ls (next[.] x) y))))