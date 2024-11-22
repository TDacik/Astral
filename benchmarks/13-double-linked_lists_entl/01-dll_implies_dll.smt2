(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x DLS_t)
(declare-const y DLS_t)

(assert (dls x y nil nil))
(assert (not (dls x y nil nil)))

(check-sat)
