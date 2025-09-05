(set-info :source Astral)
(set-info :status sat)

; def-begin
; def-end

(declare-const x Ref_SKL)

(assert
  (sep
    (distinct x nil)
    (skl2 x nil)
  )
)

(assert (not (pto x (c_SKL nil nil))))

(check-sat)
