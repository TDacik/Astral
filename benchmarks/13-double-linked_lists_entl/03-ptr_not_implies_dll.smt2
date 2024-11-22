(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x DLS_t)
(declare-const y DLS_t)

(assert (pto x (c_dls nil nil)))
(assert (not (dls x y nil nil)))

(check-sat)
