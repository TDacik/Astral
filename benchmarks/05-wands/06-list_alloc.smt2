(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert (ls x y))

; x is allocated
(assert
  (wand
    (pto x (as sep.nil Loc))
    false
  )
)

(check-sat)
