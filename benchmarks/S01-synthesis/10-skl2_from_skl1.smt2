(declare-sort Ref_SKL 0)

(declare-datatype SKL ((c_SKL (lv1 Ref_SKL) (lv2 Ref_SKL))))

(declare-heap (Ref_SKL SKL))

(define-fun-rec skl1 ((x Ref_SKL) (y Ref_SKL)) Bool
  (or
    (= x y)
    (exists ((n Ref_SKL))
      (sep
        (distinct x y)
        (pto x (c_SKL n nil))
        (skl1 n y)
      )
    )
  )
)

(declare-const x Ref_SKL)
(declare-const y Ref_SKL)

(assert (! (exists ((n1 Ref_SKL) (n2 Ref_SKL))
  (sep
    (distinct x y nil)
    (pto x  (c_SKL n1 n2))
    (skl1 n1 n2)
    (pto n2 (c_SKL y y))
  )
) :positive-example))

(compute-abstraction)
