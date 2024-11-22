(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

; Heap can be split into three non-empty sub-heaps
;   => its size at least 3
(assert
  (sep
    (not sep.emp)
    (not sep.emp)
    (not sep.emp)
  )
)

(check-sat)
