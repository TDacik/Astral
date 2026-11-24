(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

; Empty heap can be extended to something
(assert
  (septraction
    sep.emp
    true
  )
)

(check-sat)
