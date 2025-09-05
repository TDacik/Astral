(set-info :source Astral)
(set-info :status unsat)

; def-begin

;; Included from 00-definition.smt2 (modify there and run scripts/include_definitions.py)
;; Definition of a tree with a single hole representing left-most leaf

(declare-sort Ref_TREE 0)

(declare-datatype TREE ((c_TREE (left Ref_TREE) (right Ref_TREE))))

(declare-heap (Ref_TREE TREE))

(define-fun-rec tree ((x Ref_TREE) (ll Ref_TREE)) Bool
  (or
    (= x ll)
    (exists ((l Ref_TREE) (r Ref_TREE))
      (sep
        (distinct x ll)
	(pto x (c_TREE l r))
	(tree l ll)
	(tree r nil)
      )
    )
  )
)


; def-end

(declare-const x Ref_TREE)
(declare-const y Ref_TREE)

(assert
  (sep
    (tree x y)
    (tree y nil)
  )
)

(assert (not (tree x nil)))

(check-sat)
