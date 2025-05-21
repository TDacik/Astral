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
