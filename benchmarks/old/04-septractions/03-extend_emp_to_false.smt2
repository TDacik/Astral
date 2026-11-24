(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

; Empty heap cannot be extended
(assert
  (septraction
    sep.emp
    false
  )
)

(check-sat)
