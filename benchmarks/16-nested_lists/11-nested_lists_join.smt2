(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x1 NLS_t)
(declare-const x2 NLS_t)

(declare-const y Loc)

(assert
  (sep
    (pto x1 (c_nls x2 y))
    (pto x2 (c_nls nil y))
    (pto y nil)
  )
)


(assert (not (nls x1 nil nil)))

(check-sat)
