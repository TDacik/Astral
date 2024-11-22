(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x1_1 NLS_t)
(declare-const x2_1 NLS_t)
(declare-const x3_1 NLS_t)

(declare-const x1_2 Loc)
(declare-const x2_2 Loc)

(declare-const x1_3 Loc)

(assert
  (sep
    (pto x1_1 (c_nls x2_1 x1_2))
    (pto x2_1 (c_nls x3_1 x2_2))
    (pto x1_2 x1_3)
    (pto x2_2 nil)
    (pto x1_3 nil)
  )
)


(assert (not (nls x1_1 x3_1 nil)))

(check-sat)
