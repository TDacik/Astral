(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const a Loc)
(declare-const b Loc)
(declare-const c Loc)
(declare-const d Loc)

(assert
  (sep
    (ls a b)
    (pto b c)
    (pto c d)
    (ls d a)
  )
)

(assert (not
  (sep
    (ls a c)
    (ls c a)
  )
))

(check-sat)
