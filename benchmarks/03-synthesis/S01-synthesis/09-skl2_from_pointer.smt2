(declare-sort Ref_SKL 0)

(declare-datatype SKL ((c_SKL (lv1 Ref_SKL) (lv2 Ref_SKL))))

(declare-heap (Ref_SKL SKL))

(declare-const x Ref_SKL)
(declare-const y Ref_SKL)

(assert (! (sep (= x y)) :positive-example))

(assert (! (exists ((n1 Ref_SKL))
  (sep
    (distinct x y n1 nil)
    (pto x  (c_SKL n1 nil))
    (pto n1 (c_SKL y nil))
  )
) :positive-example))

(compute-abstraction)
