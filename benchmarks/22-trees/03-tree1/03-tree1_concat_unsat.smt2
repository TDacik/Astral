(set-info :source Astral)
(set-info :status unsat)

; def-begin
; def-end

(declare-const x Ref_TREE)
(declare-const y Ref_TREE)

(assert
  (sep
    (tree1 x y)
    (tree1 y nil)
  )
)

(assert (not (tree1 x nil)))

(check-sat)
