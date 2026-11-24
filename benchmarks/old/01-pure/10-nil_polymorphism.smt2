(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x_ls  LS_t)
(declare-const x_dls DLS_t)
(declare-const x_nls NLS_t)

(assert (= nil x_ls x_dls x_nls))

(check-sat)
