(set-info :status unknown)

(declare-const footprint0 (Set (_ BitVec 2)))
(declare-const nil (_ BitVec 2))
(declare-const x (_ BitVec 2))
(declare-const y (_ BitVec 2))
(declare-const Ref_LS (Set (_ BitVec 2)))
(declare-const field_next (Array (_ BitVec 2) (_ BitVec 2)))

(assert
 (and (= x y) (= footprint0 set) (not (mem (as nil Bool) footprint0))
  (= #b00 (as nil Bitvector2)) (mem x Ref_LS) (mem y Ref_LS)
  (= Ref_LS (set #b00 #b01)) (= #b00 (select field_next #b00))))