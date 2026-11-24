(set-info :status unknown)

(declare-const footprint0 (_ BitVec 2))
(declare-const nil (_ BitVec 2))
(declare-const x (_ BitVec 2))
(declare-const y (_ BitVec 2))
(declare-const Ref_LS (_ BitVec 2))
(declare-const Ref_NLS (_ BitVec 2))
(declare-const field_down (Array (_ BitVec 2) (_ BitVec 2)))
(declare-const field_top (Array (_ BitVec 2) (_ BitVec 2)))
(declare-const field_next (Array (_ BitVec 2) (_ BitVec 2)))

(assert
 (and (= x y) (= footprint0 #b00) (not (bit-check footprint0 (as nil Bool)))
  (= #b00 (as nil Bitvector2)) (bit-check Ref_NLS x) (bit-check Ref_NLS y)
  (= Ref_LS (>> #b01 #b00))
  (= Ref_NLS (bit-or (>> #b01 #b00) (>> #b01 #b01)))
  (= #b00 (select field_down #b00)) (= #b00 (select field_top #b00))
  (= #b00 (select field_next #b00))))