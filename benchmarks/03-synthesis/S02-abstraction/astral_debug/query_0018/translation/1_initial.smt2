(set-info :status unknown)

(declare-const footprint2 (Set (_ BitVec 2)))
(declare-const footprint3 (Set (_ BitVec 2)))
(declare-const footprint0 (Set (_ BitVec 2)))
(declare-const nil (_ BitVec 2))
(declare-const x (_ BitVec 2))
(declare-const y (_ BitVec 2))
(declare-const z (_ BitVec 2))
(declare-const Ref_LS (Set (_ BitVec 2)))
(declare-const field_next (Array (_ BitVec 2) (_ BitVec 2)))

(assert
 (and (= x y) (= footprint2 set) (= y z) (= footprint3 set)
  (disjoint footprint2 footprint3)
  (= footprint0 (union footprint2 footprint3))
  (not (mem (as nil Bool) footprint0)) (= #b00 (as nil Bitvector2))
  (mem x Ref_LS) (mem y Ref_LS) (mem z Ref_LS) (= Ref_LS (set #b00 #b01))
  (= #b00 (select field_next #b00))))