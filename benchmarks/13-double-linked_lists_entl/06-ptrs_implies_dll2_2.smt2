(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x DLS_t)
(declare-const y DLS_t)

(assert
  (sep
    (pto x (c_dls y nil))
    (pto y (c_dls nil x))
  )
)

(assert (not (dls x y nil nil)))

(check-sat)
