(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert
  (wand
    (pto x (as sep.nil Loc))
    false
  )
)

(assert
  (wand
    (pto y (as sep.nil Loc))
    false
  )
)

(check-sat)

; Tests
(set-info :alloc x)
(set-info :alloc y)
