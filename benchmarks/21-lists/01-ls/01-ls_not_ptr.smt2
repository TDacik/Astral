(set-info :source Astral)
(set-info :status sat)

; def-begin
; def-end

(declare-const x Ref_LS)

(assert
  (sep
    (distinct x nil)
    (ls x nil)
  )
)

(assert (not (pto x (c_LS nil))))

(check-sat)
