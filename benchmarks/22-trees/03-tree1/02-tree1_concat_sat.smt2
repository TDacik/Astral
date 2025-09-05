(set-info :source Astral)
(set-info :status sat)

; def-begin

;; Included from 00-definition.smt2 (modify there and run scripts/include_definitions.py)
;; Definition of a tree with single hole

(declare-sort Ref_TREE 0)

(declare-datatype TREE ((c_TREE (left Ref_TREE) (right Ref_TREE))))

(declare-heap (Ref_TREE TREE))

(define-fun-rec tree1 ((x Ref_TREE) (h1 Ref_TREE)) Bool
  (or
    (= x h1)
    (exists ((l Ref_TREE) (r Ref_TREE))
      (sep
        (distinct x h1)
        (pto x (c_TREE l r))
        (tree1 l h1)
        (tree1 r nil)
      )
    )
    (exists ((l Ref_TREE) (r Ref_TREE))
      (sep
        (distinct x h1)
        (pto x (c_TREE l r))
        (tree1 l nil)
        (tree1 r h1)
      )
    )
  )
)


; def-end

(declare-const x Ref_TREE)
(declare-const y Ref_TREE)
(declare-const z Ref_TREE)

(assert
  (sep
    (tree1 x y)
    (tree1 y z)
  )
)

(assert (not (tree1 x z)))

(check-sat)
