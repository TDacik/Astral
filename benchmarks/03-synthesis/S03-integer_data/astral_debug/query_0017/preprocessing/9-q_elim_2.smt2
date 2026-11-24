(set-info :status unknown)

(declare-const y Ref_LS)
(declare-const x Ref_LS)

(assert
 (sep (distinct x (next[.] x)) (distinct x y)
  (distinct (data[.] x) (data[.] (next[.] x))) (distinct (next[.] x) y)
  (distinct x (next[.] x)) (distinct x y) (distinct (next[.] x) y)
  (pto x (c_LS (next[.] x) (data[.] x)))
  (pto (next[.] x) (c_LS y (data[.] (next[.] x))))))

(assert
 (not
  (sep (pto x (c_LS (next[.] x) (data[.] x)))
   (sep (pto (next[.] x) (c_LS (next[.] (next[.] x)) (data[.] (next[.] x))))
    emp))))