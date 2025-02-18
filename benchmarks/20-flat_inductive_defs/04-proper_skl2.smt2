(set-info :source Astral)
(set-info :status sat)

(declare-sort Ref_SKL 0)

(declare-datatype SKL ((c_SKL (next1 Ref_SKL) (next2 Ref_SKL))))

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

(define-fun-rec skl2 ((x Ref_SKL) (y Ref_SKL)) Bool
  (or
    (= x y)
    (exists ((n1 Ref_SKL) (n2 Ref_SKL))
      (sep
        (distinct x y)
	(pto x (c_SKL n1 n2))
	(skl1 n1 n2)
	(skl2 n2 y)
      )
    )
  )
)

(declare-const x Ref_SKL)
(declare-const y Ref_SKL)

(assert
  (sep
    (distinct x y)
    (skl2 x y)
  )
)

(assert (not (pto x (c_SKL y y))))

(check-sat)
