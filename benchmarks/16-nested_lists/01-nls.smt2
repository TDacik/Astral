(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x NLS_t)
(declare-const y NLS_t)

(assert (nls x y nil))

(check-sat)
