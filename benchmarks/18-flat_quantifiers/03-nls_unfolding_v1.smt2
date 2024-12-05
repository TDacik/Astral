(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x NLS_t)
(declare-const y NLS_t)
(declare-const z NLS_t)

(assert
  (and
    (sep
      (nls x y z)
      (distinct x y)
    )
    (not
      (pto x (c_nls y z))
    )
  )
)

(assert (not
  (exists ((n Loc))
    (sep
      (pto x (c_nls y n))
      (pto n z)
    )
  )
))

(check-sat)
