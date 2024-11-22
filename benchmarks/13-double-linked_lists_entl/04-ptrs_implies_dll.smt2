(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x DLS_t)
(declare-const y DLS_t)

(assert
  (sep
    (pto x (c_dls y x))
    (pto y (c_dls y x))
  )
)

(assert (not (dls x y x y)))

(check-sat)
