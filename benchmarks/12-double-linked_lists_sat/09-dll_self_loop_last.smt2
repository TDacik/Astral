(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x DLS_t)
(declare-const y DLS_t)

(assert (dls x y nil y))

(check-sat)
