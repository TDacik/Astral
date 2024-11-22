(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

; Empty heap can be extended to a non-empty heap
(assert
  (septraction
    sep.emp
    (not sep.emp)
  )
)

(check-sat)
