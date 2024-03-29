(set-info :source Astral)
(set-info :status sat)

(declare-sort DLS_t 0)
(declare-heap (DLS_t DLS_t))

(declare-const x DLS_t)
(declare-const y DLS_t)

(assert
  (sep
    (pto x (c_dls y x))
    (pto y (c_dls y x))
  )
)

(assert (not (dls x y x y)))

(check-sat)
