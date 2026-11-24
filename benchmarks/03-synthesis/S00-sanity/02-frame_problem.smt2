;; Both formulae contain atom x -> next[x] which cannot be
;; used as a frame.

(declare-sort Ref_LS 0)

(declare-datatype LS ((c_LS (next Ref_LS))))

(declare-heap (Ref_LS LS))

(declare-const x Ref_LS)
(declare-const y Ref_LS)

(declare-const unused Ref_LS)

(assert (! (exists ((n1 Ref_LS))
  (sep
    (distinct x n1 y)
    (pto x (c_LS n1))
    (pto n1 (c_LS y))
    (distinct unused nil)
  )
) :positive-example))

(assert (! (exists ((n1 Ref_LS) (n2 Ref_LS))
  (sep
    (distinct x n1 n2 y)
    (pto x (c_LS n1))
    (pto n1 (c_LS n2))
    (pto n2 (c_LS y))
    (= unused nil)
  )
) :positive-example))

(compute-abstraction)
