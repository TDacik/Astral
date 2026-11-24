(set-info :status unknown)

(declare-const footprint31 (_ BitVec 7))
(declare-const footprint32 (_ BitVec 7))
(declare-const footprint29 (_ BitVec 7))
(declare-const footprint30 (_ BitVec 7))
(declare-const footprint27 (_ BitVec 7))
(declare-const footprint28 (_ BitVec 7))
(declare-const footprint2 (_ BitVec 7))
(declare-const footprint3 (_ BitVec 7))
(declare-const footprint4 (_ BitVec 7))
(declare-const footprint5 (_ BitVec 7))
(declare-const footprint6 (_ BitVec 7))
(declare-const footprint7 (_ BitVec 7))
(declare-const footprint8 (_ BitVec 7))
(declare-const footprint9 (_ BitVec 7))
(declare-const footprint10 (_ BitVec 7))
(declare-const footprint11 (_ BitVec 7))
(declare-const footprint12 (_ BitVec 7))
(declare-const footprint13 (_ BitVec 7))
(declare-const footprint14 (_ BitVec 7))
(declare-const footprint15 (_ BitVec 7))
(declare-const footprint16 (_ BitVec 7))
(declare-const footprint17 (_ BitVec 7))
(declare-const footprint18 (_ BitVec 7))
(declare-const footprint19 (_ BitVec 7))
(declare-const footprint20 (_ BitVec 7))
(declare-const footprint21 (_ BitVec 7))
(declare-const footprint22 (_ BitVec 7))
(declare-const footprint23 (_ BitVec 7))
(declare-const footprint24 (_ BitVec 7))
(declare-const footprint25 (_ BitVec 7))
(declare-const footprint26 (_ BitVec 7))
(declare-const footprint0 (_ BitVec 7))
(declare-const nil (_ BitVec 7))
(declare-const y (_ BitVec 7))
(declare-const x (_ BitVec 7))
(declare-const z (_ BitVec 7))
(declare-const Ref_LS (_ BitVec 7))
(declare-const Ref_NLS (_ BitVec 7))
(declare-const field_down (Array (_ BitVec 7) (_ BitVec 7)))
(declare-const field_top (Array (_ BitVec 7) (_ BitVec 7)))
(declare-const field_next (Array (_ BitVec 7) (_ BitVec 7)))

(assert
 (and (distinct (as nil Bitvector7) x) (= footprint2 #b0000000)
  (distinct (as nil Bitvector7) (select field_top x))
  (= footprint3 #b0000000) (distinct (as nil Bitvector7) y)
  (= footprint4 #b0000000)
  (distinct (as nil Bitvector7) (select field_down x))
  (= footprint5 #b0000000)
  (distinct (as nil Bitvector7) (select field_down (select field_top x)))
  (= footprint6 #b0000000) (distinct (as nil Bitvector7) z)
  (= footprint7 #b0000000) (distinct x (select field_top x))
  (= footprint8 #b0000000) (distinct x y) (= footprint9 #b0000000)
  (distinct (select field_top x) y) (= footprint10 #b0000000)
  (distinct (select field_down x) (select field_down (select field_top x)))
  (= footprint11 #b0000000) (distinct (select field_down x) z)
  (= footprint12 #b0000000)
  (distinct (select field_down (select field_top x)) z)
  (= footprint13 #b0000000) (distinct x (select field_top x))
  (= footprint14 #b0000000) (distinct x y) (= footprint15 #b0000000)
  (distinct (select field_top x) y) (= footprint16 #b0000000)
  (distinct (select field_down x) z) (= footprint17 #b0000000)
  (distinct (select field_down x) (as nil Bitvector7))
  (= footprint18 #b0000000) (distinct z (as nil Bitvector7))
  (= footprint19 #b0000000)
  (distinct (select field_down (select field_top x)) z)
  (= footprint20 #b0000000)
  (distinct (select field_down (select field_top x)) (as nil Bitvector7))
  (= footprint21 #b0000000) (distinct z (as nil Bitvector7))
  (= footprint22 #b0000000) (= footprint23 (>> #b0000001 x))
  (= footprint24 (>> #b0000001 (select field_top x)))
  (= (select field_top (select field_top x)) y)
  (= footprint25 (>> #b0000001 (select field_down x)))
  (= (select field_next (select field_down x)) z)
  (ite (= (select field_down (select field_top x)) z)
   (= footprint26 #b0000000)
   (and
    (= footprint27 (>> #b0000001 (select field_down (select field_top x))))
    (ite (= (select field_next (select field_down (select field_top x))) z)
     (= footprint28 #b0000000)
     (and
      (= footprint29
       (>> #b0000001
        (select field_next (select field_down (select field_top x)))))
      (ite
       (=
        (select field_next
         (select field_next (select field_down (select field_top x))))
        z)
       (= footprint30 #b0000000)
       (and
        (= footprint31
         (>> #b0000001
          (select field_next
           (select field_next (select field_down (select field_top x))))))
        (=
         (select field_next
          (select field_next
           (select field_next (select field_down (select field_top x)))))
         z)
        (= footprint32 #b0000000)
        (= (bit-and footprint31 footprint32) #b0000000)
        (= footprint30 (bit-or footprint31 footprint32))))
      (= (bit-and footprint29 footprint30) #b0000000)
      (= footprint28 (bit-or footprint29 footprint30))))
    (= (bit-and footprint27 footprint28) #b0000000)
    (= footprint26 (bit-or footprint27 footprint28))))
  (and (= (bit-and footprint25 footprint26) #b0000000)
   (= (bit-and footprint24 footprint26) #b0000000)
   (= (bit-and footprint24 footprint25) #b0000000)
   (= (bit-and footprint23 footprint26) #b0000000)
   (= (bit-and footprint23 footprint25) #b0000000)
   (= (bit-and footprint23 footprint24) #b0000000)
   (= (bit-and footprint22 footprint26) #b0000000)
   (= (bit-and footprint22 footprint25) #b0000000)
   (= (bit-and footprint22 footprint24) #b0000000)
   (= (bit-and footprint22 footprint23) #b0000000)
   (= (bit-and footprint21 footprint26) #b0000000)
   (= (bit-and footprint21 footprint25) #b0000000)
   (= (bit-and footprint21 footprint24) #b0000000)
   (= (bit-and footprint21 footprint23) #b0000000)
   (= (bit-and footprint21 footprint22) #b0000000)
   (= (bit-and footprint20 footprint26) #b0000000)
   (= (bit-and footprint20 footprint25) #b0000000)
   (= (bit-and footprint20 footprint24) #b0000000)
   (= (bit-and footprint20 footprint23) #b0000000)
   (= (bit-and footprint20 footprint22) #b0000000)
   (= (bit-and footprint20 footprint21) #b0000000)
   (= (bit-and footprint19 footprint26) #b0000000)
   (= (bit-and footprint19 footprint25) #b0000000)
   (= (bit-and footprint19 footprint24) #b0000000)
   (= (bit-and footprint19 footprint23) #b0000000)
   (= (bit-and footprint19 footprint22) #b0000000)
   (= (bit-and footprint19 footprint21) #b0000000)
   (= (bit-and footprint19 footprint20) #b0000000)
   (= (bit-and footprint18 footprint26) #b0000000)
   (= (bit-and footprint18 footprint25) #b0000000)
   (= (bit-and footprint18 footprint24) #b0000000)
   (= (bit-and footprint18 footprint23) #b0000000)
   (= (bit-and footprint18 footprint22) #b0000000)
   (= (bit-and footprint18 footprint21) #b0000000)
   (= (bit-and footprint18 footprint20) #b0000000)
   (= (bit-and footprint18 footprint19) #b0000000)
   (= (bit-and footprint17 footprint26) #b0000000)
   (= (bit-and footprint17 footprint25) #b0000000)
   (= (bit-and footprint17 footprint24) #b0000000)
   (= (bit-and footprint17 footprint23) #b0000000)
   (= (bit-and footprint17 footprint22) #b0000000)
   (= (bit-and footprint17 footprint21) #b0000000)
   (= (bit-and footprint17 footprint20) #b0000000)
   (= (bit-and footprint17 footprint19) #b0000000)
   (= (bit-and footprint17 footprint18) #b0000000)
   (= (bit-and footprint16 footprint26) #b0000000)
   (= (bit-and footprint16 footprint25) #b0000000)
   (= (bit-and footprint16 footprint24) #b0000000)
   (= (bit-and footprint16 footprint23) #b0000000)
   (= (bit-and footprint16 footprint22) #b0000000)
   (= (bit-and footprint16 footprint21) #b0000000)
   (= (bit-and footprint16 footprint20) #b0000000)
   (= (bit-and footprint16 footprint19) #b0000000)
   (= (bit-and footprint16 footprint18) #b0000000)
   (= (bit-and footprint16 footprint17) #b0000000)
   (= (bit-and footprint15 footprint26) #b0000000)
   (= (bit-and footprint15 footprint25) #b0000000)
   (= (bit-and footprint15 footprint24) #b0000000)
   (= (bit-and footprint15 footprint23) #b0000000)
   (= (bit-and footprint15 footprint22) #b0000000)
   (= (bit-and footprint15 footprint21) #b0000000)
   (= (bit-and footprint15 footprint20) #b0000000)
   (= (bit-and footprint15 footprint19) #b0000000)
   (= (bit-and footprint15 footprint18) #b0000000)
   (= (bit-and footprint15 footprint17) #b0000000)
   (= (bit-and footprint15 footprint16) #b0000000)
   (= (bit-and footprint14 footprint26) #b0000000)
   (= (bit-and footprint14 footprint25) #b0000000)
   (= (bit-and footprint14 footprint24) #b0000000)
   (= (bit-and footprint14 footprint23) #b0000000)
   (= (bit-and footprint14 footprint22) #b0000000)
   (= (bit-and footprint14 footprint21) #b0000000)
   (= (bit-and footprint14 footprint20) #b0000000)
   (= (bit-and footprint14 footprint19) #b0000000)
   (= (bit-and footprint14 footprint18) #b0000000)
   (= (bit-and footprint14 footprint17) #b0000000)
   (= (bit-and footprint14 footprint16) #b0000000)
   (= (bit-and footprint14 footprint15) #b0000000)
   (= (bit-and footprint13 footprint26) #b0000000)
   (= (bit-and footprint13 footprint25) #b0000000)
   (= (bit-and footprint13 footprint24) #b0000000)
   (= (bit-and footprint13 footprint23) #b0000000)
   (= (bit-and footprint13 footprint22) #b0000000)
   (= (bit-and footprint13 footprint21) #b0000000)
   (= (bit-and footprint13 footprint20) #b0000000)
   (= (bit-and footprint13 footprint19) #b0000000)
   (= (bit-and footprint13 footprint18) #b0000000)
   (= (bit-and footprint13 footprint17) #b0000000)
   (= (bit-and footprint13 footprint16) #b0000000)
   (= (bit-and footprint13 footprint15) #b0000000)
   (= (bit-and footprint13 footprint14) #b0000000)
   (= (bit-and footprint12 footprint26) #b0000000)
   (= (bit-and footprint12 footprint25) #b0000000)
   (= (bit-and footprint12 footprint24) #b0000000)
   (= (bit-and footprint12 footprint23) #b0000000)
   (= (bit-and footprint12 footprint22) #b0000000)
   (= (bit-and footprint12 footprint21) #b0000000)
   (= (bit-and footprint12 footprint20) #b0000000)
   (= (bit-and footprint12 footprint19) #b0000000)
   (= (bit-and footprint12 footprint18) #b0000000)
   (= (bit-and footprint12 footprint17) #b0000000)
   (= (bit-and footprint12 footprint16) #b0000000)
   (= (bit-and footprint12 footprint15) #b0000000)
   (= (bit-and footprint12 footprint14) #b0000000)
   (= (bit-and footprint12 footprint13) #b0000000)
   (= (bit-and footprint11 footprint26) #b0000000)
   (= (bit-and footprint11 footprint25) #b0000000)
   (= (bit-and footprint11 footprint24) #b0000000)
   (= (bit-and footprint11 footprint23) #b0000000)
   (= (bit-and footprint11 footprint22) #b0000000)
   (= (bit-and footprint11 footprint21) #b0000000)
   (= (bit-and footprint11 footprint20) #b0000000)
   (= (bit-and footprint11 footprint19) #b0000000)
   (= (bit-and footprint11 footprint18) #b0000000)
   (= (bit-and footprint11 footprint17) #b0000000)
   (= (bit-and footprint11 footprint16) #b0000000)
   (= (bit-and footprint11 footprint15) #b0000000)
   (= (bit-and footprint11 footprint14) #b0000000)
   (= (bit-and footprint11 footprint13) #b0000000)
   (= (bit-and footprint11 footprint12) #b0000000)
   (= (bit-and footprint10 footprint26) #b0000000)
   (= (bit-and footprint10 footprint25) #b0000000)
   (= (bit-and footprint10 footprint24) #b0000000)
   (= (bit-and footprint10 footprint23) #b0000000)
   (= (bit-and footprint10 footprint22) #b0000000)
   (= (bit-and footprint10 footprint21) #b0000000)
   (= (bit-and footprint10 footprint20) #b0000000)
   (= (bit-and footprint10 footprint19) #b0000000)
   (= (bit-and footprint10 footprint18) #b0000000)
   (= (bit-and footprint10 footprint17) #b0000000)
   (= (bit-and footprint10 footprint16) #b0000000)
   (= (bit-and footprint10 footprint15) #b0000000)
   (= (bit-and footprint10 footprint14) #b0000000)
   (= (bit-and footprint10 footprint13) #b0000000)
   (= (bit-and footprint10 footprint12) #b0000000)
   (= (bit-and footprint10 footprint11) #b0000000)
   (= (bit-and footprint9 footprint26) #b0000000)
   (= (bit-and footprint9 footprint25) #b0000000)
   (= (bit-and footprint9 footprint24) #b0000000)
   (= (bit-and footprint9 footprint23) #b0000000)
   (= (bit-and footprint9 footprint22) #b0000000)
   (= (bit-and footprint9 footprint21) #b0000000)
   (= (bit-and footprint9 footprint20) #b0000000)
   (= (bit-and footprint9 footprint19) #b0000000)
   (= (bit-and footprint9 footprint18) #b0000000)
   (= (bit-and footprint9 footprint17) #b0000000)
   (= (bit-and footprint9 footprint16) #b0000000)
   (= (bit-and footprint9 footprint15) #b0000000)
   (= (bit-and footprint9 footprint14) #b0000000)
   (= (bit-and footprint9 footprint13) #b0000000)
   (= (bit-and footprint9 footprint12) #b0000000)
   (= (bit-and footprint9 footprint11) #b0000000)
   (= (bit-and footprint9 footprint10) #b0000000)
   (= (bit-and footprint8 footprint26) #b0000000)
   (= (bit-and footprint8 footprint25) #b0000000)
   (= (bit-and footprint8 footprint24) #b0000000)
   (= (bit-and footprint8 footprint23) #b0000000)
   (= (bit-and footprint8 footprint22) #b0000000)
   (= (bit-and footprint8 footprint21) #b0000000)
   (= (bit-and footprint8 footprint20) #b0000000)
   (= (bit-and footprint8 footprint19) #b0000000)
   (= (bit-and footprint8 footprint18) #b0000000)
   (= (bit-and footprint8 footprint17) #b0000000)
   (= (bit-and footprint8 footprint16) #b0000000)
   (= (bit-and footprint8 footprint15) #b0000000)
   (= (bit-and footprint8 footprint14) #b0000000)
   (= (bit-and footprint8 footprint13) #b0000000)
   (= (bit-and footprint8 footprint12) #b0000000)
   (= (bit-and footprint8 footprint11) #b0000000)
   (= (bit-and footprint8 footprint10) #b0000000)
   (= (bit-and footprint8 footprint9) #b0000000)
   (= (bit-and footprint7 footprint26) #b0000000)
   (= (bit-and footprint7 footprint25) #b0000000)
   (= (bit-and footprint7 footprint24) #b0000000)
   (= (bit-and footprint7 footprint23) #b0000000)
   (= (bit-and footprint7 footprint22) #b0000000)
   (= (bit-and footprint7 footprint21) #b0000000)
   (= (bit-and footprint7 footprint20) #b0000000)
   (= (bit-and footprint7 footprint19) #b0000000)
   (= (bit-and footprint7 footprint18) #b0000000)
   (= (bit-and footprint7 footprint17) #b0000000)
   (= (bit-and footprint7 footprint16) #b0000000)
   (= (bit-and footprint7 footprint15) #b0000000)
   (= (bit-and footprint7 footprint14) #b0000000)
   (= (bit-and footprint7 footprint13) #b0000000)
   (= (bit-and footprint7 footprint12) #b0000000)
   (= (bit-and footprint7 footprint11) #b0000000)
   (= (bit-and footprint7 footprint10) #b0000000)
   (= (bit-and footprint7 footprint9) #b0000000)
   (= (bit-and footprint7 footprint8) #b0000000)
   (= (bit-and footprint6 footprint26) #b0000000)
   (= (bit-and footprint6 footprint25) #b0000000)
   (= (bit-and footprint6 footprint24) #b0000000)
   (= (bit-and footprint6 footprint23) #b0000000)
   (= (bit-and footprint6 footprint22) #b0000000)
   (= (bit-and footprint6 footprint21) #b0000000)
   (= (bit-and footprint6 footprint20) #b0000000)
   (= (bit-and footprint6 footprint19) #b0000000)
   (= (bit-and footprint6 footprint18) #b0000000)
   (= (bit-and footprint6 footprint17) #b0000000)
   (= (bit-and footprint6 footprint16) #b0000000)
   (= (bit-and footprint6 footprint15) #b0000000)
   (= (bit-and footprint6 footprint14) #b0000000)
   (= (bit-and footprint6 footprint13) #b0000000)
   (= (bit-and footprint6 footprint12) #b0000000)
   (= (bit-and footprint6 footprint11) #b0000000)
   (= (bit-and footprint6 footprint10) #b0000000)
   (= (bit-and footprint6 footprint9) #b0000000)
   (= (bit-and footprint6 footprint8) #b0000000)
   (= (bit-and footprint6 footprint7) #b0000000)
   (= (bit-and footprint5 footprint26) #b0000000)
   (= (bit-and footprint5 footprint25) #b0000000)
   (= (bit-and footprint5 footprint24) #b0000000)
   (= (bit-and footprint5 footprint23) #b0000000)
   (= (bit-and footprint5 footprint22) #b0000000)
   (= (bit-and footprint5 footprint21) #b0000000)
   (= (bit-and footprint5 footprint20) #b0000000)
   (= (bit-and footprint5 footprint19) #b0000000)
   (= (bit-and footprint5 footprint18) #b0000000)
   (= (bit-and footprint5 footprint17) #b0000000)
   (= (bit-and footprint5 footprint16) #b0000000)
   (= (bit-and footprint5 footprint15) #b0000000)
   (= (bit-and footprint5 footprint14) #b0000000)
   (= (bit-and footprint5 footprint13) #b0000000)
   (= (bit-and footprint5 footprint12) #b0000000)
   (= (bit-and footprint5 footprint11) #b0000000)
   (= (bit-and footprint5 footprint10) #b0000000)
   (= (bit-and footprint5 footprint9) #b0000000)
   (= (bit-and footprint5 footprint8) #b0000000)
   (= (bit-and footprint5 footprint7) #b0000000)
   (= (bit-and footprint5 footprint6) #b0000000)
   (= (bit-and footprint4 footprint26) #b0000000)
   (= (bit-and footprint4 footprint25) #b0000000)
   (= (bit-and footprint4 footprint24) #b0000000)
   (= (bit-and footprint4 footprint23) #b0000000)
   (= (bit-and footprint4 footprint22) #b0000000)
   (= (bit-and footprint4 footprint21) #b0000000)
   (= (bit-and footprint4 footprint20) #b0000000)
   (= (bit-and footprint4 footprint19) #b0000000)
   (= (bit-and footprint4 footprint18) #b0000000)
   (= (bit-and footprint4 footprint17) #b0000000)
   (= (bit-and footprint4 footprint16) #b0000000)
   (= (bit-and footprint4 footprint15) #b0000000)
   (= (bit-and footprint4 footprint14) #b0000000)
   (= (bit-and footprint4 footprint13) #b0000000)
   (= (bit-and footprint4 footprint12) #b0000000)
   (= (bit-and footprint4 footprint11) #b0000000)
   (= (bit-and footprint4 footprint10) #b0000000)
   (= (bit-and footprint4 footprint9) #b0000000)
   (= (bit-and footprint4 footprint8) #b0000000)
   (= (bit-and footprint4 footprint7) #b0000000)
   (= (bit-and footprint4 footprint6) #b0000000)
   (= (bit-and footprint4 footprint5) #b0000000)
   (= (bit-and footprint3 footprint26) #b0000000)
   (= (bit-and footprint3 footprint25) #b0000000)
   (= (bit-and footprint3 footprint24) #b0000000)
   (= (bit-and footprint3 footprint23) #b0000000)
   (= (bit-and footprint3 footprint22) #b0000000)
   (= (bit-and footprint3 footprint21) #b0000000)
   (= (bit-and footprint3 footprint20) #b0000000)
   (= (bit-and footprint3 footprint19) #b0000000)
   (= (bit-and footprint3 footprint18) #b0000000)
   (= (bit-and footprint3 footprint17) #b0000000)
   (= (bit-and footprint3 footprint16) #b0000000)
   (= (bit-and footprint3 footprint15) #b0000000)
   (= (bit-and footprint3 footprint14) #b0000000)
   (= (bit-and footprint3 footprint13) #b0000000)
   (= (bit-and footprint3 footprint12) #b0000000)
   (= (bit-and footprint3 footprint11) #b0000000)
   (= (bit-and footprint3 footprint10) #b0000000)
   (= (bit-and footprint3 footprint9) #b0000000)
   (= (bit-and footprint3 footprint8) #b0000000)
   (= (bit-and footprint3 footprint7) #b0000000)
   (= (bit-and footprint3 footprint6) #b0000000)
   (= (bit-and footprint3 footprint5) #b0000000)
   (= (bit-and footprint3 footprint4) #b0000000)
   (= (bit-and footprint2 footprint26) #b0000000)
   (= (bit-and footprint2 footprint25) #b0000000)
   (= (bit-and footprint2 footprint24) #b0000000)
   (= (bit-and footprint2 footprint23) #b0000000)
   (= (bit-and footprint2 footprint22) #b0000000)
   (= (bit-and footprint2 footprint21) #b0000000)
   (= (bit-and footprint2 footprint20) #b0000000)
   (= (bit-and footprint2 footprint19) #b0000000)
   (= (bit-and footprint2 footprint18) #b0000000)
   (= (bit-and footprint2 footprint17) #b0000000)
   (= (bit-and footprint2 footprint16) #b0000000)
   (= (bit-and footprint2 footprint15) #b0000000)
   (= (bit-and footprint2 footprint14) #b0000000)
   (= (bit-and footprint2 footprint13) #b0000000)
   (= (bit-and footprint2 footprint12) #b0000000)
   (= (bit-and footprint2 footprint11) #b0000000)
   (= (bit-and footprint2 footprint10) #b0000000)
   (= (bit-and footprint2 footprint9) #b0000000)
   (= (bit-and footprint2 footprint8) #b0000000)
   (= (bit-and footprint2 footprint7) #b0000000)
   (= (bit-and footprint2 footprint6) #b0000000)
   (= (bit-and footprint2 footprint5) #b0000000)
   (= (bit-and footprint2 footprint4) #b0000000)
   (= (bit-and footprint2 footprint3) #b0000000))
  (= footprint0
   (bit-or footprint2 footprint3 footprint4 footprint5 footprint6 footprint7
    footprint8 footprint9 footprint10 footprint11 footprint12 footprint13
    footprint14 footprint15 footprint16 footprint17 footprint18 footprint19
    footprint20 footprint21 footprint22 footprint23 footprint24 footprint25
    footprint26))
  (not
   (and
    (=
     (bit-and (>> #b0000001 z)
      (ite (= (select field_next z) (as nil Bitvector7)) #b0000000
       (bit-or (>> #b0000001 (select field_next z))
        (>> #b0000001 (select field_next (select field_next z)))
        (>> #b0000001
         (select field_next (select field_next (select field_next z))))
        (>> #b0000001
         (select field_next
          (select field_next (select field_next (select field_next z)))))
        (>> #b0000001
         (select field_next
          (select field_next
           (select field_next (select field_next (select field_next z)))))))))
     #b0000000)
    (= footprint0
     (bit-or (>> #b0000001 z)
      (ite (= (select field_next z) (as nil Bitvector7)) #b0000000
       (bit-or (>> #b0000001 (select field_next z))
        (>> #b0000001 (select field_next (select field_next z)))
        (>> #b0000001
         (select field_next (select field_next (select field_next z))))
        (>> #b0000001
         (select field_next
          (select field_next (select field_next (select field_next z)))))
        (>> #b0000001
         (select field_next
          (select field_next
           (select field_next (select field_next (select field_next z))))))))))
    (= (>> #b0000001 z) (>> #b0000001 z))
    (ite (= (select field_next z) (as nil Bitvector7))
     (=
      (ite (= (select field_next z) (as nil Bitvector7)) #b0000000
       (bit-or (>> #b0000001 (select field_next z))
        (>> #b0000001 (select field_next (select field_next z)))
        (>> #b0000001
         (select field_next (select field_next (select field_next z))))
        (>> #b0000001
         (select field_next
          (select field_next (select field_next (select field_next z)))))
        (>> #b0000001
         (select field_next
          (select field_next
           (select field_next (select field_next (select field_next z))))))))
      #b0000000)
     (and
      (=
       (bit-and (>> #b0000001 (select field_next z))
        (bit-or (>> #b0000001 (select field_next (select field_next z)))
         (>> #b0000001
          (select field_next (select field_next (select field_next z))))
         (>> #b0000001
          (select field_next
           (select field_next (select field_next (select field_next z)))))
         (>> #b0000001
          (select field_next
           (select field_next
            (select field_next (select field_next (select field_next z))))))))
       #b0000000)
      (=
       (ite (= (select field_next z) (as nil Bitvector7)) #b0000000
        (bit-or (>> #b0000001 (select field_next z))
         (>> #b0000001 (select field_next (select field_next z)))
         (>> #b0000001
          (select field_next (select field_next (select field_next z))))
         (>> #b0000001
          (select field_next
           (select field_next (select field_next (select field_next z)))))
         (>> #b0000001
          (select field_next
           (select field_next
            (select field_next (select field_next (select field_next z))))))))
       (bit-or (>> #b0000001 (select field_next z))
        (>> #b0000001 (select field_next (select field_next z)))
        (>> #b0000001
         (select field_next (select field_next (select field_next z))))
        (>> #b0000001
         (select field_next
          (select field_next (select field_next (select field_next z)))))
        (>> #b0000001
         (select field_next
          (select field_next
           (select field_next (select field_next (select field_next z))))))))
      (= (>> #b0000001 (select field_next z))
       (>> #b0000001 (select field_next z)))
      (=
       (bit-and (>> #b0000001 (select field_next (select field_next z)))
        (bit-or
         (>> #b0000001
          (select field_next (select field_next (select field_next z))))
         (>> #b0000001
          (select field_next
           (select field_next (select field_next (select field_next z)))))
         (>> #b0000001
          (select field_next
           (select field_next
            (select field_next (select field_next (select field_next z))))))))
       #b0000000)
      (=
       (bit-or (>> #b0000001 (select field_next (select field_next z)))
        (>> #b0000001
         (select field_next (select field_next (select field_next z))))
        (>> #b0000001
         (select field_next
          (select field_next (select field_next (select field_next z)))))
        (>> #b0000001
         (select field_next
          (select field_next
           (select field_next (select field_next (select field_next z)))))))
       (bit-or (>> #b0000001 (select field_next (select field_next z)))
        (>> #b0000001
         (select field_next (select field_next (select field_next z))))
        (>> #b0000001
         (select field_next
          (select field_next (select field_next (select field_next z)))))
        (>> #b0000001
         (select field_next
          (select field_next
           (select field_next (select field_next (select field_next z))))))))
      (= (>> #b0000001 (select field_next (select field_next z)))
       (>> #b0000001 (select field_next (select field_next z))))
      (=
       (bit-and
        (>> #b0000001
         (select field_next (select field_next (select field_next z))))
        (bit-or
         (>> #b0000001
          (select field_next
           (select field_next (select field_next (select field_next z)))))
         (>> #b0000001
          (select field_next
           (select field_next
            (select field_next (select field_next (select field_next z))))))))
       #b0000000)
      (=
       (bit-or
        (>> #b0000001
         (select field_next (select field_next (select field_next z))))
        (>> #b0000001
         (select field_next
          (select field_next (select field_next (select field_next z)))))
        (>> #b0000001
         (select field_next
          (select field_next
           (select field_next (select field_next (select field_next z)))))))
       (bit-or
        (>> #b0000001
         (select field_next (select field_next (select field_next z))))
        (>> #b0000001
         (select field_next
          (select field_next (select field_next (select field_next z)))))
        (>> #b0000001
         (select field_next
          (select field_next
           (select field_next (select field_next (select field_next z))))))))
      (=
       (>> #b0000001
        (select field_next (select field_next (select field_next z))))
       (>> #b0000001
        (select field_next (select field_next (select field_next z)))))
      (=
       (bit-and
        (>> #b0000001
         (select field_next
          (select field_next (select field_next (select field_next z)))))
        (>> #b0000001
         (select field_next
          (select field_next
           (select field_next (select field_next (select field_next z)))))))
       #b0000000)
      (=
       (bit-or
        (>> #b0000001
         (select field_next
          (select field_next (select field_next (select field_next z)))))
        (>> #b0000001
         (select field_next
          (select field_next
           (select field_next (select field_next (select field_next z)))))))
       (bit-or
        (>> #b0000001
         (select field_next
          (select field_next (select field_next (select field_next z)))))
        (>> #b0000001
         (select field_next
          (select field_next
           (select field_next (select field_next (select field_next z))))))))
      (=
       (>> #b0000001
        (select field_next
         (select field_next (select field_next (select field_next z)))))
       (>> #b0000001
        (select field_next
         (select field_next (select field_next (select field_next z))))))
      true
      (=
       (>> #b0000001
        (select field_next
         (select field_next
          (select field_next (select field_next (select field_next z))))))
       (>> #b0000001
        (select field_next
         (select field_next
          (select field_next (select field_next (select field_next z)))))))
      (=
       (>> #b0000001
        (select field_next
         (select field_next
          (select field_next (select field_next (select field_next z))))))
       (>> #b0000001
        (select field_next
         (select field_next
          (select field_next (select field_next (select field_next z)))))))
      (=
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (select field_next z))))))
       (as nil Bitvector7))
      (= #b0000000 #b0000000)))))
  (not (bit-check footprint0 (as nil Bool)))
  (= #b0000000 (as nil Bitvector7)) (bit-check Ref_NLS x)
  (bit-check Ref_NLS (select field_top x)) (bit-check Ref_NLS y)
  (bit-check Ref_LS (select field_down x))
  (bit-check Ref_LS (select field_down (select field_top x)))
  (bit-check Ref_LS
   (select field_next (select field_down (select field_top x))))
  (bit-check Ref_LS
   (select field_next
    (select field_next (select field_down (select field_top x)))))
  (bit-check Ref_LS
   (select field_next
    (select field_next
     (select field_next (select field_down (select field_top x))))))
  (bit-check Ref_LS z) (bit-check Ref_LS (select field_next z))
  (bit-check Ref_LS (select field_next (select field_next z)))
  (bit-check Ref_LS
   (select field_next (select field_next (select field_next z))))
  (bit-check Ref_LS
   (select field_next
    (select field_next (select field_next (select field_next z)))))
  (bit-check Ref_LS
   (select field_next
    (select field_next
     (select field_next (select field_next (select field_next z))))))
  (bit-check Ref_LS
   (select field_next
    (select field_next
     (select field_next
      (select field_next (select field_next (select field_next z)))))))
  (= Ref_LS
   (bit-or (>> #b0000001 #b0000000) (>> #b0000001 #b0000001)
    (>> #b0000001 #b0000010) (>> #b0000001 #b0000011)))
  (= Ref_NLS
   (bit-or (>> #b0000001 #b0000000) (>> #b0000001 #b0000100)
    (>> #b0000001 #b0000101) (>> #b0000001 #b0000110)))
  (= #b0000000 (select field_down #b0000000))
  (= #b0000000 (select field_top #b0000000))
  (= #b0000000 (select field_next #b0000000))))