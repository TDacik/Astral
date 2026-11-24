(set-info :status unknown)

(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const z Ref_LS)

(assert
 (sep (distinct x (next[.] x)) (distinct x y) (distinct x (next[.] y))
  (distinct (next[.] x) y) (distinct (next[.] x) (next[.] y))
  (distinct y (next[.] y)) (distinct x (next[.] x)) (distinct x y)
  (distinct (next[.] x) y) (distinct y (next[.] y)) (distinct y z)
  (distinct (next[.] y) z) (pto y (c_LS (next[.] y)))
  (pto (next[.] y) (c_LS z))))

(assert (not (pred1 y nil z)))