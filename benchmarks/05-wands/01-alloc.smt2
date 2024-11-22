(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)

(assert
  (wand
    (pto x (as nil Loc))
    false
  )
)

(check-sat)

; Tests
(set-info :alloc x)
