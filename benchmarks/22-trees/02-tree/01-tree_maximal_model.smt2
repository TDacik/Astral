(set-info :source Astral)
(set-info :status sat)

; def-begin
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
