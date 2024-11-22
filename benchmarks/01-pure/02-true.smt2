(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(assert true)

(check-sat)

; Tests
(set-info :location_bound_min 1)
(set-info :location_bound_max 1)
