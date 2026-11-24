(set-info :source Astral)
(set-info :status sat)

; def-begin
; def-end

(declare-const x Ref_TREE)
(declare-const y Ref_TREE)
(declare-const z Ref_TREE)

(assert
  (sep
    (tree x y)
    (tree y z)
  )
)

(assert (not (tree x z)))

(check-sat)
