(set-info :source Astral)
(set-info :status unsat)

; def-begin

;; Included from 00-definition.smt2 (modify there and run scripts/include_definitions.py)
;; Doubly-linked list
;; TODO: add comment on semantics

(declare-sort Ref_DLS 0)

(declare-datatype DLS ((c_DLS (next Ref_DLS) (prev Ref_DLS))))

(declare-heap (Ref_DLS DLS))

; Doubly-linked list:
;   --------------------------
;   y'  <-  x  <-> ... <->  x'  ->  y
;           -------------------------

(define-fun-rec dls ((x Ref_DLS) (y Ref_DLS) (xp Ref_DLS) (yp Ref_DLS)) Bool
  (or
    (sep (= x y) (= xp yp))
    (exists ((n Ref_DLS))
      (sep
        (distinct x y)
	(distinct xp yp)
	(pto x (c_DLS n yp))
	(dls n y xp x)
      )
    )
  )
)


; def-end

; The following model is a valid DLS according to standard semantics
; used SL-comp:
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
