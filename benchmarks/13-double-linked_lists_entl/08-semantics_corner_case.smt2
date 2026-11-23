(set-info :source Astral)
(set-info :status sat)

(include "predicates/doubly_linked_list.smt2")

; The following model is a valid DLS according to standard semantics
; used SL-comp. However, our built-in semantics does not allow it.
;
;    _______
;   |      v
;   `---[x, yp] <-> xp -> y
;


(declare-const x Ref_DLS)
(declare-const y Ref_DLS)
(declare-const xp DLS_t)
(declare-const yp DLS_t)

(assert
  (sep
    (pto x (c_DLS xp yp))
    (pto xp (c_DLS y x))
    (= x yp)
    (distinct x xp y)
  )
)

(assert (not (dls x xp yp y)))

(check-sat)
