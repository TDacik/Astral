(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x1 NLS_t)
(declare-const x2 NLS_t)

(assert
  (sep
    (distinct x1 x2)
    (pto x1 (c_nls x2 nil))
  )
)

(assert (not (nls x1 x2 nil)))

(check-sat)
