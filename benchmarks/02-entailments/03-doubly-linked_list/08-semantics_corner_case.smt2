(set-info :source Astral)
(set-info :status unsat)

(include "predicates/doubly-linked_list.smt2")

; The following model is a valid DLS according to standard semantics
; used in SL-comp:
;
;    _______
;   |      v
;   `---[x, yp] <-> xp -> y
;

(declare-const x Ref_DLS)
(declare-const y Ref_DLS)
(declare-const xp Ref_DLS)
(declare-const yp Ref_DLS)

(assert
  (sep
    (pto x (c_DLS xp yp))
    (pto xp (c_DLS y x))
    (= x yp)
    (distinct x xp y)
  )
)

(assert (not (dls x y xp yp)))

(check-sat)
