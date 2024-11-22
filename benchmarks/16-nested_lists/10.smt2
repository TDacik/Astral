(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x1 NLS_t)
(declare-const x1_1 Loc)

(assert
  (sep
    (pto x1 (c_nls nil x1_1))
    (distinct x1_1 nil)
    (ls x1_1 nil)
  )
)


(assert (not (nls x1 nil nil)))

(check-sat)
