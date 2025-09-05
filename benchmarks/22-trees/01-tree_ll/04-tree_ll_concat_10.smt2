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
(declare-const h1 Ref_TREE)
(declare-const h2 Ref_TREE)
(declare-const h3 Ref_TREE)
(declare-const h4 Ref_TREE)
(declare-const h5 Ref_TREE)
(declare-const h6 Ref_TREE)
(declare-const h7 Ref_TREE)
(declare-const h8 Ref_TREE)
(declare-const h9 Ref_TREE)

(assert
  (sep
    (tree x h1)
    (tree h1 h2)
    (tree h2 h3)
    (tree h3 h4)
    (tree h4 h5)
    (tree h5 h6)
    (tree h6 h7)
    (tree h7 h8)
    (tree h8 h9)
    (tree h9 nil)
  )
)

(assert (not (tree x nil)))

(check-sat)
