(set-info :source Astral)
(set-info :status sat)

; def-begin

;; Included from 00-def.smt2 (modify there and run scripts/include_definitions.py)
;; Tree without holes

(declare-sort Ref_TREE 0)

(declare-datatype TREE ((c_TREE (left Ref_TREE) (right Ref_TREE))))

(declare-heap (Ref_TREE TREE))

(define-fun-rec tree ((x Ref_TREE)) Bool
  (or
    (= x nil)
    (exists ((l Ref_TREE) (r Ref_TREE))
      (sep
        (distinct x nil)
	(pto x (c_TREE l r))
	(tree l)
	(tree r)
      )
    )
  )
)


; def-end

(declare-const x Ref_TREE)

(assert
  (sep
    (distinct x nil)
    (tree x)
  )
)

(assert (not (pto x (c_TREE nil nil))))

(check-sat)
