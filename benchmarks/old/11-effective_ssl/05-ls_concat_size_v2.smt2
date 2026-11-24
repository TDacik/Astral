(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert
  (sep
    ;; ls 1+
    (and (ls x y) (not emp))
    ;; ls 2+
    (and (ls y nil) (sep (not emp) (not emp)))
  )
)

(assert (not
  ;; ls 4+
  (and (ls x nil) (sep (not emp) (not emp) (not emp) (not emp)))
))

(check-sat)
