(declare-sort Ref_SL 0)
(declare-sort Ref_SLL 0)

(declare-datatype SL ((c_SL (next Ref_SLL))))
(declare-datatype SLL ((c_SLL (next Ref_SLL) (loop Ref_SLL))))

(declare-heap (Ref_SL SL) (Ref_SLL SLL))

(declare-const h Ref_SL)
(declare-const x Ref_SLL)
(declare-const y Ref_SLL)

(assert (! (sep (= x y)) :positive-example))

(assert (! (exists ((n1 Ref_SLL))
  (sep
    (distinct x y)
    (pto h  (c_SL x))
    (pto x  (c_SLL n1 x))
    (pto n1 (c_SLL y n1))
  )
) :positive-example))

(compute-abstraction)
