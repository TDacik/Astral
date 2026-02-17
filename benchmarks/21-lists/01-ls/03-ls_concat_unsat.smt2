(set-info :source Astral)
(set-info :status unsat)

; def-begin

;; Included from 00-definition.smt2 (modify there and run scripts/include_definitions.py)
; Singly-linked list

(declare-sort Ref_LS 0)

(declare-datatype LS ((c_LS (next Ref_LS))))

(declare-heap (Ref_LS LS))

(define-fun-rec ls ((x Ref_LS) (y Ref_LS)) Bool
  (or
    (= x y)
    (exists ((n Ref_LS))
      (sep
        (distinct x y)
        (pto x (c_LS n))
        (ls n y)
      )
    )
  )
)


; def-end

(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert
  (sep
    (ls x y)
    (ls y nil)
  )
)

(assert (not (ls x nil)))

(check-sat)
