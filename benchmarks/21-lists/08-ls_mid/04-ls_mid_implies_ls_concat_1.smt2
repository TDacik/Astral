(set-info :source Astral)
(set-info :status unsat)

; def-begin
; def-end

(declare-const x Ref_LS)
(declare-const m Ref_LS)
(declare-const y Ref_LS)

(assert
  (ls_mid x m y)
)

(assert (not
  (sep
    (ls_mid x x m)
    (ls_mid m m y)
  )
))

(check-sat)
