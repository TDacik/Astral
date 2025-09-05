(set-info :source Astral)
(set-info :status sat)

; def-begin
; def-end

(declare-const x1 Ref_LS)
(declare-const y1 Ref_LS)
(declare-const z1 Ref_LS)

(declare-const x2 Ref_LS)
(declare-const y2 Ref_LS)

(assert
  (sep
    (ls x1 y1)
    (ls y1 z1)

    (ls x2 y2)
    (ls y2 nil)

  )
)

(assert (not
  (sep
    (ls x1 z1)
    (ls x2 nil)
  )
))

(check-sat)
