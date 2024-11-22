(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x DLS_t)

(assert (pto x (c_dls nil nil)))
(assert (not (dls x x nil nil)))

(check-sat)
