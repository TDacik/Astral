(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x DLS_t)
(declare-const y DLS_t)

(assert
  (sep
    (distinct x y)
    (dls x y x nil)
  )
)

(check-sat)
