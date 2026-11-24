(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(assert (not true))

(check-sat)

; Tests
(set-info :location_bound_min 1)
(set-info :location_bound_max 1)
