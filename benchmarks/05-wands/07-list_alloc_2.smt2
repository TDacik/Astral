(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const z Loc)
(declare-const y Loc)

(assert
  (sep
    (distinct x z)
    (ls x y)
  )
)

; x is allocated
(assert
  (wand
    (pto x (as sep.nil Loc))
    false
  )
)

; z is allocated
(assert
  (wand
    (pto z (as sep.nil Loc))
    false
  )
)

(check-sat)
