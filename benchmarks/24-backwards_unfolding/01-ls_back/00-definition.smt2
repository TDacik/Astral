; Singly-linked list defined by unfolding from the end

(declare-sort Ref_LS 0)

(declare-datatype LS ((c_LS (next Ref_LS))))

(declare-heap (Ref_LS LS))

(define-fun-rec ls_back ((x Ref_LS) (y Ref_LS)) Bool
  (or
    (= x y)
    (exists ((p Ref_LS))
      (sep
        (distinct x y)
	(pto p (c_LS y))
	(ls_back x p)
      )
    )
  )
)
