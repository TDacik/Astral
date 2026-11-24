(set-info :status unknown)

(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert
 (sep (distinct x (next[.] x)) (distinct x (next[.] (next[.] x)))
  (distinct x y) (distinct (data[.] x) (data[.] (next[.] x)))
  (distinct (data[.] x) (data[.] (next[.] (next[.] x))))
  (distinct (next[.] x) (next[.] (next[.] x))) (distinct (next[.] x) y)
  (distinct (data[.] (next[.] x)) (data[.] (next[.] (next[.] x))))
  (distinct (next[.] (next[.] x)) y) (distinct x (next[.] x))
  (distinct x (next[.] (next[.] x))) (distinct x y)
  (distinct (next[.] x) (next[.] (next[.] x))) (distinct (next[.] x) y)
  (distinct (next[.] (next[.] x)) y) (pto x (c_LS (next[.] x) (data[.] x)))
  (pto (next[.] x) (c_LS (next[.] (next[.] x)) (data[.] (next[.] x))))
  (pto (next[.] (next[.] x)) (c_LS y (data[.] (next[.] (next[.] x)))))))

(assert (not (pred1 x y)))