(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

; Something can be extended to the empty heap
(assert
  (septraction
    true
    sep.emp
  )
)

(check-sat)
