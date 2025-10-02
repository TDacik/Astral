(set-info :source Astral)
(set-info :status unsat)

; def-begin
; def-end

(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert
  (ls_mid x y x)
)

(assert (not
  (ls_mid y x y)
))

(check-sat)
