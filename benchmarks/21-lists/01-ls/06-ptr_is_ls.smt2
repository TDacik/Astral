(set-info :source Astral)
(set-info :status unsat)

; def-begin
; def-end


(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert
  (sep
    (pto x (c_LS y))
    (distinct x y)
  )
)

(assert (not (ls x y)))

(check-sat)
