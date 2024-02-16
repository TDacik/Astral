(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

; Heap can be split into four non-empty sub-heaps
;   => its size is at least 4
(assert
  (sep
    (not sep.emp)
    (not sep.emp)
    (not sep.emp)
    (not sep.emp)
  )
)

; Heap cannot be split into three non-empty sub-heaps
;   => its size is at most 2
(assert
  (not
    (sep
      (not sep.emp)
      (not sep.emp)
      (not sep.emp)
    )
  )
)

(check-sat)
