(set-info :source Astral)
(set-info :status sat)

; def-begin
; def-end

(declare-const x Ref_NLS)

(assert
  (sep
    (distinct x nil)
    (nls x nil nil)
  )
)

(assert (not (pto x (c_NLS nil nil))))

(check-sat)
