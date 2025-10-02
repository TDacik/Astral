(set-info :source Astral)
(set-info :status sat)

; def-begin
; def-end

(declare-const x Ref_LS)
(declare-const m Ref_LS)
(declare-const y Ref_LS)

(assert
  (ls_mid x m y)
)

(assert (not
  (pto x (c_LS y))
))

(check-sat)
