(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

; Empty heap can be extended to a pointer x -> y
(assert
  (septraction
    sep.emp
    (pto x y)
  )
)

(check-sat)
