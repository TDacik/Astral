(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x1 Loc)
(declare-const y1 Loc)

(declare-const x2 Loc)
(declare-const y2 Loc)

; A pointer x1 -> y1 can be extended to pointer x2 -> y2 (if x1 = x2 and y1 = y2)
(assert
  (septraction
    (pto x1 y1)
    (pto x2 y2)
  )
)

(check-sat)
