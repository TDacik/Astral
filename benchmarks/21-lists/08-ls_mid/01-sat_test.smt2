(set-info :source Astral)
(set-info :status sat)

; def-begin

;; Included from 00-definition.smt2 (modify there and run scripts/include_definitions.py)
; Singly-linked list with named midpoint:
;
;   ls_mid(x, m, y) <=> (ls(x,m) * ls(m,y)) /\ ls(x, y)

(declare-sort Ref_LS 0)

(declare-datatype LS ((c_LS (next Ref_LS))))

(declare-heap (Ref_LS LS))

(define-fun-rec ls_mid ((x Ref_LS) (m Ref_LS) (y Ref_LS)) Bool
  (or
    (= x m y)
    (exists ((n Ref_LS))
      (sep
        (= x m)
	(distinct x y)
        (pto x (c_LS n))
        (ls_mid n n y)
      )
    )
    (exists ((n Ref_LS))
      (sep
        (distinct x m y)
        (pto x (c_LS n))
        (ls_mid n m y)
      )
    )
  )
)


; def-end

(declare-const x Ref_LS)
(declare-const m Ref_LS)
(declare-const y Ref_LS)

(assert
  (ls_mid x m y)
)

(check-sat)
