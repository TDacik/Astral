(set-info :source Astral)
(set-info :status sat)

; def-begin
; def-end

(declare-const x Ref_SKL)

(assert
  (sep
    (distinct x nil)
    (skl3 x nil)
  )
)

(assert (not (skl2 x nil)))

(check-sat)
