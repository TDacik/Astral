(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x DLS_t)
(declare-const y DLS_t)
(declare-const z DLS_t)

(assert
  (sep
    (pto x (c_dls y nil))
    (pto y (c_dls z x))
    (pto z (c_dls nil y))
  )
)

(assert (not (dls x z nil nil)))

(check-sat)
