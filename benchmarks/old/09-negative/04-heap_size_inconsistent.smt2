(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

; Heap can be split into three non-empty sub-heaps
;   => its size is at least 3
(assert
  (sep
    (not sep.emp)
    (not sep.emp)
    (not sep.emp)
  )
)

; Heap cannot be split into two non-empty sub-heaps
;   => its size is at most 1
(assert
  (not
    (sep
      (not sep.emp)
      (not sep.emp)
    )
  )
)

(check-sat)
