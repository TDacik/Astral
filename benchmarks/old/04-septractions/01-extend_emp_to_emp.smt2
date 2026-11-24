(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

; Empty heap can be extended to an empty heap
(assert
  (septraction
    sep.emp
    sep.emp
  )
)

(check-sat)
