(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

; Pointer x -> y can be extended to pointer x -> y
(assert
  (septraction
    (pto x y)
    (pto x y)
  )
)

(check-sat)
