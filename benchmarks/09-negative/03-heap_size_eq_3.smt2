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

; Heap cannot be split into four non-empty sub-heaps
;   => its size at most 3
(assert
  (not
    (sep
      (not sep.emp)
      (not sep.emp)
      (not sep.emp)
      (not sep.emp)
    )
  )
)

(check-sat)
