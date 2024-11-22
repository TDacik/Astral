(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

; x is not allocated
(assert
  (septraction
    (pto x nil)
    true
  )
)

(check-sat)
