(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert
  (sep
    ;; ls 1+
    (and (ls x y) (not emp))
    ;; ls 1+
    (and (ls y nil) (not emp))
  )
)

(assert (not
  ;; ls 3+
  (and (ls x nil) (sep (not emp) (not emp) (not emp)))
))

(check-sat)
