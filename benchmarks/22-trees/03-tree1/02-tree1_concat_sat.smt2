(set-info :source Astral)
(set-info :status sat)

; def-begin
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
