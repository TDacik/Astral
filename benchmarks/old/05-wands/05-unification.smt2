(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)
(declare-const z Loc)

; The heap is empty and s(x) = s(z)
(assert
  (wand
    (pto x y)
    (pto z y)
  )
)

(check-sat)
