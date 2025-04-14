(set-info :source Astral)
(set-info :status unsat)

(declare-sort Ref_TREE 0)

(declare-datatype TREE ((c_TREE (left Ref_TREE) (right Ref_TREE))))

(declare-heap (Ref_TREE TREE))

(define-fun-rec tree ((x Ref_TREE) (h1 Ref_TREE)) Bool
  (or
    (and (= x h1) emp)
    (exists ((l Ref_TREE) (r Ref_TREE))
      (and
        (distinct x h1)
      	(sep
	  (pto x (c_TREE l r))
	  (tree l h1)
	  (tree r nil)
        )
      )
    )
    (exists ((l Ref_TREE) (r Ref_TREE))
      (and
        (distinct x h1)
      	(sep
	  (pto x (c_TREE l r))
	  (tree l nil)
	  (tree r h1)
        )
      )
    )
  )
)

(declare-const x Ref_TREE)
(declare-const h Ref_TREE)

(assert
  (sep
    (tree x h)
    (tree h nil)
  )
)

(assert (not (tree x nil)))

(check-sat)
