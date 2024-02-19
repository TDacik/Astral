(set-info :source Astral)
(set-info :status sat)

(declare-sort DLS_t 0)
(declare-heap (DLS_t DLS_t))

(declare-const x DLS_t)
(declare-const y DLS_t)

(assert
  (sep
    (distinct x y)
    (dls x y nil nil)
  )
)

(check-sat)
