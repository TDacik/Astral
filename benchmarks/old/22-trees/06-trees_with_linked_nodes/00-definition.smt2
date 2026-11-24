(declare-sort Ref_TLN 0)

(declare-datatype TLN ((c_TLN (left Ref_TLN) (right Ref_TLN) (next Ref_TLN))))

(declare-heap (Ref_TLN TLN))

(define-fun-rec tln ((root Ref_TLN) (ll Ref_TLN) (lr Ref_TLN) (aux Ref_TLN)) Bool
  (or
    (sep
      (pto root (c_TLL nil nil lr))n
      (= root ll)
      (distinct root lr)
    )
    (exists ((l Ref_TLL) (r Ref_TLL) (s Ref_TLL))
      (sep
	(distinct root lr)
	(pto root (c_TLL l r nil))
	(tll l ll s)
	(tll r s lr)
      )
    )
  )
)
