(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

; False can be extended
(assert
  (septraction
    false
    true
  )
)

(check-sat)
