(set-info :source Astral)
(set-info :status unsat)

; def-begin
; def-end

(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const z Ref_LS)

(assert
  (sep
    (ls x y)
    (ls y z)
    (ls z nil)
  )
)

(assert (not (ls x nil)))

(check-sat)
