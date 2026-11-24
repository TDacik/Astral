(set-info :source Astral)
(set-info :status sat)

(declare-heap ((_ BitVec 16) (_ BitVec 32)))

(declare-const x (_ BitVec 16))
(declare-const y (_ BitVec 32))

(assert (pto x y))

(check-sat)
