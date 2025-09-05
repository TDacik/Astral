(set-info :source Astral)
(set-info :status unsat)

; def-begin
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
