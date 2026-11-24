(set-info :status unknown)

(declare-const z Ref_LS)
(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert
 (sep (distinct x (next[.] x)) (distinct x y) (distinct x (next[.] y))
  (distinct (next[.] x) y) (distinct (next[.] x) (next[.] y))
  (distinct y (next[.] y)) (distinct x (next[.] x)) (distinct x y)
  (distinct (next[.] x) y) (distinct y (next[.] y)) (distinct y z)
  (distinct (next[.] y) z) (pto x (c_LS (next[.] x)))
  (pto (next[.] x) (c_LS y))))