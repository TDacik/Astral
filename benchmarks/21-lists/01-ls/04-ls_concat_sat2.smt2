(set-info :source Astral)
(set-info :status sat)

; def-begin
; def-end

(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const z Ref_LS)

(assert
  (sep
    (distinct x y z)
    (ls x y)
    (ls y z)
  )
)

(assert (not (ls x z)))

(check-sat)
