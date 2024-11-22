(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x DLS_t)
(declare-const y DLS_t)

(assert
  (sep
    (distinct x y)
    (distinct x nil)
    (dls x y nil nil)
  )
)

(assert (not
  (sep
    (pto x (c_dls y nil))
    (pto y (c_dls nil x))
  )
))

(check-sat)
