(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x NLS_t)
(declare-const y NLS_t)

(assert (nls x y nil))
(assert (not (nls x y nil)))

(check-sat)
