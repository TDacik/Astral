(set-info :source Astral)
(set-info :status sat)

; def-begin
; def-end

(declare-const w Ref_LS)
(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const z Ref_LS)

(assert
  (sep
    (ls x y)
    (ls y z)
    (ls z w)
  )
)

(assert (not (ls x w)))

(check-sat)
