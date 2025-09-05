(set-info :source Astral)
(set-info :status sat)

; def-begin
; def-end

(declare-const x Ref_DNLS)
(declare-const x_r Ref_DNLS)

(assert
  (sep
    (distinct x x_r nil)
    (dnls x nil x_r nil nil)
  )
)

(assert (not
  (sep
    (pto x (c_DNLS x_r nil nil))
    (pto x_r (c_DNLS nil x nil))
  )
))

(check-sat)
