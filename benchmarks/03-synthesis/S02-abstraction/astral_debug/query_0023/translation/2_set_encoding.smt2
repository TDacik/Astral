(set-info :status unknown)

(declare-const footprint2 (_ BitVec 8))
(declare-const footprint3 (_ BitVec 8))
(declare-const footprint4 (_ BitVec 8))
(declare-const footprint5 (_ BitVec 8))
(declare-const footprint6 (_ BitVec 8))
(declare-const footprint7 (_ BitVec 8))
(declare-const footprint8 (_ BitVec 8))
(declare-const footprint9 (_ BitVec 8))
(declare-const footprint10 (_ BitVec 8))
(declare-const footprint11 (_ BitVec 8))
(declare-const footprint12 (_ BitVec 8))
(declare-const footprint13 (_ BitVec 8))
(declare-const footprint14 (_ BitVec 8))
(declare-const footprint15 (_ BitVec 8))
(declare-const footprint16 (_ BitVec 8))
(declare-const footprint17 (_ BitVec 8))
(declare-const footprint18 (_ BitVec 8))
(declare-const footprint19 (_ BitVec 8))
(declare-const footprint20 (_ BitVec 8))
(declare-const footprint21 (_ BitVec 8))
(declare-const footprint22 (_ BitVec 8))
(declare-const footprint23 (_ BitVec 8))
(declare-const footprint24 (_ BitVec 8))
(declare-const footprint25 (_ BitVec 8))
(declare-const footprint26 (_ BitVec 8))
(declare-const footprint27 (_ BitVec 8))
(declare-const footprint28 (_ BitVec 8))
(declare-const footprint29 (_ BitVec 8))
(declare-const footprint30 (_ BitVec 8))
(declare-const footprint31 (_ BitVec 8))
(declare-const footprint0 (_ BitVec 8))
(declare-const y (_ BitVec 8))
(declare-const z (_ BitVec 8))
(declare-const nil (_ BitVec 8))
(declare-const x (_ BitVec 8))
(declare-const Ref_LS (_ BitVec 8))
(declare-const field_next (Array (_ BitVec 8) (_ BitVec 8)))

(assert
 (and (distinct x (select field_next x)) (= footprint2 #b00000000)
  (distinct x (select field_next (select field_next x)))
  (= footprint3 #b00000000) (distinct x y) (= footprint4 #b00000000)
  (distinct x (select field_next y)) (= footprint5 #b00000000)
  (distinct x (select field_next (select field_next y)))
  (= footprint6 #b00000000)
  (distinct (select field_next x) (select field_next (select field_next x)))
  (= footprint7 #b00000000) (distinct (select field_next x) y)
  (= footprint8 #b00000000)
  (distinct (select field_next x) (select field_next y))
  (= footprint9 #b00000000)
  (distinct (select field_next x) (select field_next (select field_next y)))
  (= footprint10 #b00000000)
  (distinct (select field_next (select field_next x)) y)
  (= footprint11 #b00000000)
  (distinct (select field_next (select field_next x)) (select field_next y))
  (= footprint12 #b00000000)
  (distinct (select field_next (select field_next x))
   (select field_next (select field_next y)))
  (= footprint13 #b00000000) (distinct y (select field_next y))
  (= footprint14 #b00000000)
  (distinct y (select field_next (select field_next y)))
  (= footprint15 #b00000000)
  (distinct (select field_next y) (select field_next (select field_next y)))
  (= footprint16 #b00000000) (distinct x (select field_next x))
  (= footprint17 #b00000000)
  (distinct x (select field_next (select field_next x)))
  (= footprint18 #b00000000) (distinct x y) (= footprint19 #b00000000)
  (distinct (select field_next x) (select field_next (select field_next x)))
  (= footprint20 #b00000000) (distinct (select field_next x) y)
  (= footprint21 #b00000000)
  (distinct (select field_next (select field_next x)) y)
  (= footprint22 #b00000000) (distinct y (select field_next y))
  (= footprint23 #b00000000)
  (distinct y (select field_next (select field_next y)))
  (= footprint24 #b00000000) (distinct y z) (= footprint25 #b00000000)
  (distinct (select field_next y) (select field_next (select field_next y)))
  (= footprint26 #b00000000) (distinct (select field_next y) z)
  (= footprint27 #b00000000)
  (distinct (select field_next (select field_next y)) z)
  (= footprint28 #b00000000) (= footprint29 (>> #b00000001 y))
  (= footprint30 (>> #b00000001 (select field_next y)))
  (= footprint31 (>> #b00000001 (select field_next (select field_next y))))
  (= (select field_next (select field_next (select field_next y))) z)
  (and (= (bit-and footprint30 footprint31) #b00000000)
   (= (bit-and footprint29 footprint31) #b00000000)
   (= (bit-and footprint29 footprint30) #b00000000)
   (= (bit-and footprint28 footprint31) #b00000000)
   (= (bit-and footprint28 footprint30) #b00000000)
   (= (bit-and footprint28 footprint29) #b00000000)
   (= (bit-and footprint27 footprint31) #b00000000)
   (= (bit-and footprint27 footprint30) #b00000000)
   (= (bit-and footprint27 footprint29) #b00000000)
   (= (bit-and footprint27 footprint28) #b00000000)
   (= (bit-and footprint26 footprint31) #b00000000)
   (= (bit-and footprint26 footprint30) #b00000000)
   (= (bit-and footprint26 footprint29) #b00000000)
   (= (bit-and footprint26 footprint28) #b00000000)
   (= (bit-and footprint26 footprint27) #b00000000)
   (= (bit-and footprint25 footprint31) #b00000000)
   (= (bit-and footprint25 footprint30) #b00000000)
   (= (bit-and footprint25 footprint29) #b00000000)
   (= (bit-and footprint25 footprint28) #b00000000)
   (= (bit-and footprint25 footprint27) #b00000000)
   (= (bit-and footprint25 footprint26) #b00000000)
   (= (bit-and footprint24 footprint31) #b00000000)
   (= (bit-and footprint24 footprint30) #b00000000)
   (= (bit-and footprint24 footprint29) #b00000000)
   (= (bit-and footprint24 footprint28) #b00000000)
   (= (bit-and footprint24 footprint27) #b00000000)
   (= (bit-and footprint24 footprint26) #b00000000)
   (= (bit-and footprint24 footprint25) #b00000000)
   (= (bit-and footprint23 footprint31) #b00000000)
   (= (bit-and footprint23 footprint30) #b00000000)
   (= (bit-and footprint23 footprint29) #b00000000)
   (= (bit-and footprint23 footprint28) #b00000000)
   (= (bit-and footprint23 footprint27) #b00000000)
   (= (bit-and footprint23 footprint26) #b00000000)
   (= (bit-and footprint23 footprint25) #b00000000)
   (= (bit-and footprint23 footprint24) #b00000000)
   (= (bit-and footprint22 footprint31) #b00000000)
   (= (bit-and footprint22 footprint30) #b00000000)
   (= (bit-and footprint22 footprint29) #b00000000)
   (= (bit-and footprint22 footprint28) #b00000000)
   (= (bit-and footprint22 footprint27) #b00000000)
   (= (bit-and footprint22 footprint26) #b00000000)
   (= (bit-and footprint22 footprint25) #b00000000)
   (= (bit-and footprint22 footprint24) #b00000000)
   (= (bit-and footprint22 footprint23) #b00000000)
   (= (bit-and footprint21 footprint31) #b00000000)
   (= (bit-and footprint21 footprint30) #b00000000)
   (= (bit-and footprint21 footprint29) #b00000000)
   (= (bit-and footprint21 footprint28) #b00000000)
   (= (bit-and footprint21 footprint27) #b00000000)
   (= (bit-and footprint21 footprint26) #b00000000)
   (= (bit-and footprint21 footprint25) #b00000000)
   (= (bit-and footprint21 footprint24) #b00000000)
   (= (bit-and footprint21 footprint23) #b00000000)
   (= (bit-and footprint21 footprint22) #b00000000)
   (= (bit-and footprint20 footprint31) #b00000000)
   (= (bit-and footprint20 footprint30) #b00000000)
   (= (bit-and footprint20 footprint29) #b00000000)
   (= (bit-and footprint20 footprint28) #b00000000)
   (= (bit-and footprint20 footprint27) #b00000000)
   (= (bit-and footprint20 footprint26) #b00000000)
   (= (bit-and footprint20 footprint25) #b00000000)
   (= (bit-and footprint20 footprint24) #b00000000)
   (= (bit-and footprint20 footprint23) #b00000000)
   (= (bit-and footprint20 footprint22) #b00000000)
   (= (bit-and footprint20 footprint21) #b00000000)
   (= (bit-and footprint19 footprint31) #b00000000)
   (= (bit-and footprint19 footprint30) #b00000000)
   (= (bit-and footprint19 footprint29) #b00000000)
   (= (bit-and footprint19 footprint28) #b00000000)
   (= (bit-and footprint19 footprint27) #b00000000)
   (= (bit-and footprint19 footprint26) #b00000000)
   (= (bit-and footprint19 footprint25) #b00000000)
   (= (bit-and footprint19 footprint24) #b00000000)
   (= (bit-and footprint19 footprint23) #b00000000)
   (= (bit-and footprint19 footprint22) #b00000000)
   (= (bit-and footprint19 footprint21) #b00000000)
   (= (bit-and footprint19 footprint20) #b00000000)
   (= (bit-and footprint18 footprint31) #b00000000)
   (= (bit-and footprint18 footprint30) #b00000000)
   (= (bit-and footprint18 footprint29) #b00000000)
   (= (bit-and footprint18 footprint28) #b00000000)
   (= (bit-and footprint18 footprint27) #b00000000)
   (= (bit-and footprint18 footprint26) #b00000000)
   (= (bit-and footprint18 footprint25) #b00000000)
   (= (bit-and footprint18 footprint24) #b00000000)
   (= (bit-and footprint18 footprint23) #b00000000)
   (= (bit-and footprint18 footprint22) #b00000000)
   (= (bit-and footprint18 footprint21) #b00000000)
   (= (bit-and footprint18 footprint20) #b00000000)
   (= (bit-and footprint18 footprint19) #b00000000)
   (= (bit-and footprint17 footprint31) #b00000000)
   (= (bit-and footprint17 footprint30) #b00000000)
   (= (bit-and footprint17 footprint29) #b00000000)
   (= (bit-and footprint17 footprint28) #b00000000)
   (= (bit-and footprint17 footprint27) #b00000000)
   (= (bit-and footprint17 footprint26) #b00000000)
   (= (bit-and footprint17 footprint25) #b00000000)
   (= (bit-and footprint17 footprint24) #b00000000)
   (= (bit-and footprint17 footprint23) #b00000000)
   (= (bit-and footprint17 footprint22) #b00000000)
   (= (bit-and footprint17 footprint21) #b00000000)
   (= (bit-and footprint17 footprint20) #b00000000)
   (= (bit-and footprint17 footprint19) #b00000000)
   (= (bit-and footprint17 footprint18) #b00000000)
   (= (bit-and footprint16 footprint31) #b00000000)
   (= (bit-and footprint16 footprint30) #b00000000)
   (= (bit-and footprint16 footprint29) #b00000000)
   (= (bit-and footprint16 footprint28) #b00000000)
   (= (bit-and footprint16 footprint27) #b00000000)
   (= (bit-and footprint16 footprint26) #b00000000)
   (= (bit-and footprint16 footprint25) #b00000000)
   (= (bit-and footprint16 footprint24) #b00000000)
   (= (bit-and footprint16 footprint23) #b00000000)
   (= (bit-and footprint16 footprint22) #b00000000)
   (= (bit-and footprint16 footprint21) #b00000000)
   (= (bit-and footprint16 footprint20) #b00000000)
   (= (bit-and footprint16 footprint19) #b00000000)
   (= (bit-and footprint16 footprint18) #b00000000)
   (= (bit-and footprint16 footprint17) #b00000000)
   (= (bit-and footprint15 footprint31) #b00000000)
   (= (bit-and footprint15 footprint30) #b00000000)
   (= (bit-and footprint15 footprint29) #b00000000)
   (= (bit-and footprint15 footprint28) #b00000000)
   (= (bit-and footprint15 footprint27) #b00000000)
   (= (bit-and footprint15 footprint26) #b00000000)
   (= (bit-and footprint15 footprint25) #b00000000)
   (= (bit-and footprint15 footprint24) #b00000000)
   (= (bit-and footprint15 footprint23) #b00000000)
   (= (bit-and footprint15 footprint22) #b00000000)
   (= (bit-and footprint15 footprint21) #b00000000)
   (= (bit-and footprint15 footprint20) #b00000000)
   (= (bit-and footprint15 footprint19) #b00000000)
   (= (bit-and footprint15 footprint18) #b00000000)
   (= (bit-and footprint15 footprint17) #b00000000)
   (= (bit-and footprint15 footprint16) #b00000000)
   (= (bit-and footprint14 footprint31) #b00000000)
   (= (bit-and footprint14 footprint30) #b00000000)
   (= (bit-and footprint14 footprint29) #b00000000)
   (= (bit-and footprint14 footprint28) #b00000000)
   (= (bit-and footprint14 footprint27) #b00000000)
   (= (bit-and footprint14 footprint26) #b00000000)
   (= (bit-and footprint14 footprint25) #b00000000)
   (= (bit-and footprint14 footprint24) #b00000000)
   (= (bit-and footprint14 footprint23) #b00000000)
   (= (bit-and footprint14 footprint22) #b00000000)
   (= (bit-and footprint14 footprint21) #b00000000)
   (= (bit-and footprint14 footprint20) #b00000000)
   (= (bit-and footprint14 footprint19) #b00000000)
   (= (bit-and footprint14 footprint18) #b00000000)
   (= (bit-and footprint14 footprint17) #b00000000)
   (= (bit-and footprint14 footprint16) #b00000000)
   (= (bit-and footprint14 footprint15) #b00000000)
   (= (bit-and footprint13 footprint31) #b00000000)
   (= (bit-and footprint13 footprint30) #b00000000)
   (= (bit-and footprint13 footprint29) #b00000000)
   (= (bit-and footprint13 footprint28) #b00000000)
   (= (bit-and footprint13 footprint27) #b00000000)
   (= (bit-and footprint13 footprint26) #b00000000)
   (= (bit-and footprint13 footprint25) #b00000000)
   (= (bit-and footprint13 footprint24) #b00000000)
   (= (bit-and footprint13 footprint23) #b00000000)
   (= (bit-and footprint13 footprint22) #b00000000)
   (= (bit-and footprint13 footprint21) #b00000000)
   (= (bit-and footprint13 footprint20) #b00000000)
   (= (bit-and footprint13 footprint19) #b00000000)
   (= (bit-and footprint13 footprint18) #b00000000)
   (= (bit-and footprint13 footprint17) #b00000000)
   (= (bit-and footprint13 footprint16) #b00000000)
   (= (bit-and footprint13 footprint15) #b00000000)
   (= (bit-and footprint13 footprint14) #b00000000)
   (= (bit-and footprint12 footprint31) #b00000000)
   (= (bit-and footprint12 footprint30) #b00000000)
   (= (bit-and footprint12 footprint29) #b00000000)
   (= (bit-and footprint12 footprint28) #b00000000)
   (= (bit-and footprint12 footprint27) #b00000000)
   (= (bit-and footprint12 footprint26) #b00000000)
   (= (bit-and footprint12 footprint25) #b00000000)
   (= (bit-and footprint12 footprint24) #b00000000)
   (= (bit-and footprint12 footprint23) #b00000000)
   (= (bit-and footprint12 footprint22) #b00000000)
   (= (bit-and footprint12 footprint21) #b00000000)
   (= (bit-and footprint12 footprint20) #b00000000)
   (= (bit-and footprint12 footprint19) #b00000000)
   (= (bit-and footprint12 footprint18) #b00000000)
   (= (bit-and footprint12 footprint17) #b00000000)
   (= (bit-and footprint12 footprint16) #b00000000)
   (= (bit-and footprint12 footprint15) #b00000000)
   (= (bit-and footprint12 footprint14) #b00000000)
   (= (bit-and footprint12 footprint13) #b00000000)
   (= (bit-and footprint11 footprint31) #b00000000)
   (= (bit-and footprint11 footprint30) #b00000000)
   (= (bit-and footprint11 footprint29) #b00000000)
   (= (bit-and footprint11 footprint28) #b00000000)
   (= (bit-and footprint11 footprint27) #b00000000)
   (= (bit-and footprint11 footprint26) #b00000000)
   (= (bit-and footprint11 footprint25) #b00000000)
   (= (bit-and footprint11 footprint24) #b00000000)
   (= (bit-and footprint11 footprint23) #b00000000)
   (= (bit-and footprint11 footprint22) #b00000000)
   (= (bit-and footprint11 footprint21) #b00000000)
   (= (bit-and footprint11 footprint20) #b00000000)
   (= (bit-and footprint11 footprint19) #b00000000)
   (= (bit-and footprint11 footprint18) #b00000000)
   (= (bit-and footprint11 footprint17) #b00000000)
   (= (bit-and footprint11 footprint16) #b00000000)
   (= (bit-and footprint11 footprint15) #b00000000)
   (= (bit-and footprint11 footprint14) #b00000000)
   (= (bit-and footprint11 footprint13) #b00000000)
   (= (bit-and footprint11 footprint12) #b00000000)
   (= (bit-and footprint10 footprint31) #b00000000)
   (= (bit-and footprint10 footprint30) #b00000000)
   (= (bit-and footprint10 footprint29) #b00000000)
   (= (bit-and footprint10 footprint28) #b00000000)
   (= (bit-and footprint10 footprint27) #b00000000)
   (= (bit-and footprint10 footprint26) #b00000000)
   (= (bit-and footprint10 footprint25) #b00000000)
   (= (bit-and footprint10 footprint24) #b00000000)
   (= (bit-and footprint10 footprint23) #b00000000)
   (= (bit-and footprint10 footprint22) #b00000000)
   (= (bit-and footprint10 footprint21) #b00000000)
   (= (bit-and footprint10 footprint20) #b00000000)
   (= (bit-and footprint10 footprint19) #b00000000)
   (= (bit-and footprint10 footprint18) #b00000000)
   (= (bit-and footprint10 footprint17) #b00000000)
   (= (bit-and footprint10 footprint16) #b00000000)
   (= (bit-and footprint10 footprint15) #b00000000)
   (= (bit-and footprint10 footprint14) #b00000000)
   (= (bit-and footprint10 footprint13) #b00000000)
   (= (bit-and footprint10 footprint12) #b00000000)
   (= (bit-and footprint10 footprint11) #b00000000)
   (= (bit-and footprint9 footprint31) #b00000000)
   (= (bit-and footprint9 footprint30) #b00000000)
   (= (bit-and footprint9 footprint29) #b00000000)
   (= (bit-and footprint9 footprint28) #b00000000)
   (= (bit-and footprint9 footprint27) #b00000000)
   (= (bit-and footprint9 footprint26) #b00000000)
   (= (bit-and footprint9 footprint25) #b00000000)
   (= (bit-and footprint9 footprint24) #b00000000)
   (= (bit-and footprint9 footprint23) #b00000000)
   (= (bit-and footprint9 footprint22) #b00000000)
   (= (bit-and footprint9 footprint21) #b00000000)
   (= (bit-and footprint9 footprint20) #b00000000)
   (= (bit-and footprint9 footprint19) #b00000000)
   (= (bit-and footprint9 footprint18) #b00000000)
   (= (bit-and footprint9 footprint17) #b00000000)
   (= (bit-and footprint9 footprint16) #b00000000)
   (= (bit-and footprint9 footprint15) #b00000000)
   (= (bit-and footprint9 footprint14) #b00000000)
   (= (bit-and footprint9 footprint13) #b00000000)
   (= (bit-and footprint9 footprint12) #b00000000)
   (= (bit-and footprint9 footprint11) #b00000000)
   (= (bit-and footprint9 footprint10) #b00000000)
   (= (bit-and footprint8 footprint31) #b00000000)
   (= (bit-and footprint8 footprint30) #b00000000)
   (= (bit-and footprint8 footprint29) #b00000000)
   (= (bit-and footprint8 footprint28) #b00000000)
   (= (bit-and footprint8 footprint27) #b00000000)
   (= (bit-and footprint8 footprint26) #b00000000)
   (= (bit-and footprint8 footprint25) #b00000000)
   (= (bit-and footprint8 footprint24) #b00000000)
   (= (bit-and footprint8 footprint23) #b00000000)
   (= (bit-and footprint8 footprint22) #b00000000)
   (= (bit-and footprint8 footprint21) #b00000000)
   (= (bit-and footprint8 footprint20) #b00000000)
   (= (bit-and footprint8 footprint19) #b00000000)
   (= (bit-and footprint8 footprint18) #b00000000)
   (= (bit-and footprint8 footprint17) #b00000000)
   (= (bit-and footprint8 footprint16) #b00000000)
   (= (bit-and footprint8 footprint15) #b00000000)
   (= (bit-and footprint8 footprint14) #b00000000)
   (= (bit-and footprint8 footprint13) #b00000000)
   (= (bit-and footprint8 footprint12) #b00000000)
   (= (bit-and footprint8 footprint11) #b00000000)
   (= (bit-and footprint8 footprint10) #b00000000)
   (= (bit-and footprint8 footprint9) #b00000000)
   (= (bit-and footprint7 footprint31) #b00000000)
   (= (bit-and footprint7 footprint30) #b00000000)
   (= (bit-and footprint7 footprint29) #b00000000)
   (= (bit-and footprint7 footprint28) #b00000000)
   (= (bit-and footprint7 footprint27) #b00000000)
   (= (bit-and footprint7 footprint26) #b00000000)
   (= (bit-and footprint7 footprint25) #b00000000)
   (= (bit-and footprint7 footprint24) #b00000000)
   (= (bit-and footprint7 footprint23) #b00000000)
   (= (bit-and footprint7 footprint22) #b00000000)
   (= (bit-and footprint7 footprint21) #b00000000)
   (= (bit-and footprint7 footprint20) #b00000000)
   (= (bit-and footprint7 footprint19) #b00000000)
   (= (bit-and footprint7 footprint18) #b00000000)
   (= (bit-and footprint7 footprint17) #b00000000)
   (= (bit-and footprint7 footprint16) #b00000000)
   (= (bit-and footprint7 footprint15) #b00000000)
   (= (bit-and footprint7 footprint14) #b00000000)
   (= (bit-and footprint7 footprint13) #b00000000)
   (= (bit-and footprint7 footprint12) #b00000000)
   (= (bit-and footprint7 footprint11) #b00000000)
   (= (bit-and footprint7 footprint10) #b00000000)
   (= (bit-and footprint7 footprint9) #b00000000)
   (= (bit-and footprint7 footprint8) #b00000000)
   (= (bit-and footprint6 footprint31) #b00000000)
   (= (bit-and footprint6 footprint30) #b00000000)
   (= (bit-and footprint6 footprint29) #b00000000)
   (= (bit-and footprint6 footprint28) #b00000000)
   (= (bit-and footprint6 footprint27) #b00000000)
   (= (bit-and footprint6 footprint26) #b00000000)
   (= (bit-and footprint6 footprint25) #b00000000)
   (= (bit-and footprint6 footprint24) #b00000000)
   (= (bit-and footprint6 footprint23) #b00000000)
   (= (bit-and footprint6 footprint22) #b00000000)
   (= (bit-and footprint6 footprint21) #b00000000)
   (= (bit-and footprint6 footprint20) #b00000000)
   (= (bit-and footprint6 footprint19) #b00000000)
   (= (bit-and footprint6 footprint18) #b00000000)
   (= (bit-and footprint6 footprint17) #b00000000)
   (= (bit-and footprint6 footprint16) #b00000000)
   (= (bit-and footprint6 footprint15) #b00000000)
   (= (bit-and footprint6 footprint14) #b00000000)
   (= (bit-and footprint6 footprint13) #b00000000)
   (= (bit-and footprint6 footprint12) #b00000000)
   (= (bit-and footprint6 footprint11) #b00000000)
   (= (bit-and footprint6 footprint10) #b00000000)
   (= (bit-and footprint6 footprint9) #b00000000)
   (= (bit-and footprint6 footprint8) #b00000000)
   (= (bit-and footprint6 footprint7) #b00000000)
   (= (bit-and footprint5 footprint31) #b00000000)
   (= (bit-and footprint5 footprint30) #b00000000)
   (= (bit-and footprint5 footprint29) #b00000000)
   (= (bit-and footprint5 footprint28) #b00000000)
   (= (bit-and footprint5 footprint27) #b00000000)
   (= (bit-and footprint5 footprint26) #b00000000)
   (= (bit-and footprint5 footprint25) #b00000000)
   (= (bit-and footprint5 footprint24) #b00000000)
   (= (bit-and footprint5 footprint23) #b00000000)
   (= (bit-and footprint5 footprint22) #b00000000)
   (= (bit-and footprint5 footprint21) #b00000000)
   (= (bit-and footprint5 footprint20) #b00000000)
   (= (bit-and footprint5 footprint19) #b00000000)
   (= (bit-and footprint5 footprint18) #b00000000)
   (= (bit-and footprint5 footprint17) #b00000000)
   (= (bit-and footprint5 footprint16) #b00000000)
   (= (bit-and footprint5 footprint15) #b00000000)
   (= (bit-and footprint5 footprint14) #b00000000)
   (= (bit-and footprint5 footprint13) #b00000000)
   (= (bit-and footprint5 footprint12) #b00000000)
   (= (bit-and footprint5 footprint11) #b00000000)
   (= (bit-and footprint5 footprint10) #b00000000)
   (= (bit-and footprint5 footprint9) #b00000000)
   (= (bit-and footprint5 footprint8) #b00000000)
   (= (bit-and footprint5 footprint7) #b00000000)
   (= (bit-and footprint5 footprint6) #b00000000)
   (= (bit-and footprint4 footprint31) #b00000000)
   (= (bit-and footprint4 footprint30) #b00000000)
   (= (bit-and footprint4 footprint29) #b00000000)
   (= (bit-and footprint4 footprint28) #b00000000)
   (= (bit-and footprint4 footprint27) #b00000000)
   (= (bit-and footprint4 footprint26) #b00000000)
   (= (bit-and footprint4 footprint25) #b00000000)
   (= (bit-and footprint4 footprint24) #b00000000)
   (= (bit-and footprint4 footprint23) #b00000000)
   (= (bit-and footprint4 footprint22) #b00000000)
   (= (bit-and footprint4 footprint21) #b00000000)
   (= (bit-and footprint4 footprint20) #b00000000)
   (= (bit-and footprint4 footprint19) #b00000000)
   (= (bit-and footprint4 footprint18) #b00000000)
   (= (bit-and footprint4 footprint17) #b00000000)
   (= (bit-and footprint4 footprint16) #b00000000)
   (= (bit-and footprint4 footprint15) #b00000000)
   (= (bit-and footprint4 footprint14) #b00000000)
   (= (bit-and footprint4 footprint13) #b00000000)
   (= (bit-and footprint4 footprint12) #b00000000)
   (= (bit-and footprint4 footprint11) #b00000000)
   (= (bit-and footprint4 footprint10) #b00000000)
   (= (bit-and footprint4 footprint9) #b00000000)
   (= (bit-and footprint4 footprint8) #b00000000)
   (= (bit-and footprint4 footprint7) #b00000000)
   (= (bit-and footprint4 footprint6) #b00000000)
   (= (bit-and footprint4 footprint5) #b00000000)
   (= (bit-and footprint3 footprint31) #b00000000)
   (= (bit-and footprint3 footprint30) #b00000000)
   (= (bit-and footprint3 footprint29) #b00000000)
   (= (bit-and footprint3 footprint28) #b00000000)
   (= (bit-and footprint3 footprint27) #b00000000)
   (= (bit-and footprint3 footprint26) #b00000000)
   (= (bit-and footprint3 footprint25) #b00000000)
   (= (bit-and footprint3 footprint24) #b00000000)
   (= (bit-and footprint3 footprint23) #b00000000)
   (= (bit-and footprint3 footprint22) #b00000000)
   (= (bit-and footprint3 footprint21) #b00000000)
   (= (bit-and footprint3 footprint20) #b00000000)
   (= (bit-and footprint3 footprint19) #b00000000)
   (= (bit-and footprint3 footprint18) #b00000000)
   (= (bit-and footprint3 footprint17) #b00000000)
   (= (bit-and footprint3 footprint16) #b00000000)
   (= (bit-and footprint3 footprint15) #b00000000)
   (= (bit-and footprint3 footprint14) #b00000000)
   (= (bit-and footprint3 footprint13) #b00000000)
   (= (bit-and footprint3 footprint12) #b00000000)
   (= (bit-and footprint3 footprint11) #b00000000)
   (= (bit-and footprint3 footprint10) #b00000000)
   (= (bit-and footprint3 footprint9) #b00000000)
   (= (bit-and footprint3 footprint8) #b00000000)
   (= (bit-and footprint3 footprint7) #b00000000)
   (= (bit-and footprint3 footprint6) #b00000000)
   (= (bit-and footprint3 footprint5) #b00000000)
   (= (bit-and footprint3 footprint4) #b00000000)
   (= (bit-and footprint2 footprint31) #b00000000)
   (= (bit-and footprint2 footprint30) #b00000000)
   (= (bit-and footprint2 footprint29) #b00000000)
   (= (bit-and footprint2 footprint28) #b00000000)
   (= (bit-and footprint2 footprint27) #b00000000)
   (= (bit-and footprint2 footprint26) #b00000000)
   (= (bit-and footprint2 footprint25) #b00000000)
   (= (bit-and footprint2 footprint24) #b00000000)
   (= (bit-and footprint2 footprint23) #b00000000)
   (= (bit-and footprint2 footprint22) #b00000000)
   (= (bit-and footprint2 footprint21) #b00000000)
   (= (bit-and footprint2 footprint20) #b00000000)
   (= (bit-and footprint2 footprint19) #b00000000)
   (= (bit-and footprint2 footprint18) #b00000000)
   (= (bit-and footprint2 footprint17) #b00000000)
   (= (bit-and footprint2 footprint16) #b00000000)
   (= (bit-and footprint2 footprint15) #b00000000)
   (= (bit-and footprint2 footprint14) #b00000000)
   (= (bit-and footprint2 footprint13) #b00000000)
   (= (bit-and footprint2 footprint12) #b00000000)
   (= (bit-and footprint2 footprint11) #b00000000)
   (= (bit-and footprint2 footprint10) #b00000000)
   (= (bit-and footprint2 footprint9) #b00000000)
   (= (bit-and footprint2 footprint8) #b00000000)
   (= (bit-and footprint2 footprint7) #b00000000)
   (= (bit-and footprint2 footprint6) #b00000000)
   (= (bit-and footprint2 footprint5) #b00000000)
   (= (bit-and footprint2 footprint4) #b00000000)
   (= (bit-and footprint2 footprint3) #b00000000))
  (= footprint0
   (bit-or footprint2 footprint3 footprint4 footprint5 footprint6 footprint7
    footprint8 footprint9 footprint10 footprint11 footprint12 footprint13
    footprint14 footprint15 footprint16 footprint17 footprint18 footprint19
    footprint20 footprint21 footprint22 footprint23 footprint24 footprint25
    footprint26 footprint27 footprint28 footprint29 footprint30 footprint31))
  (not
   (and
    (=
     (bit-and (>> #b00000001 (as nil Bitvector8))
      (bit-or (>> #b00000001 (select field_next (as nil Bitvector8)))
       (>> #b00000001
        (select field_next (select field_next (as nil Bitvector8))))
       (>> #b00000001
        (select field_next
         (select field_next (select field_next (as nil Bitvector8)))))
       (>> #b00000001
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8))))))
       (>> #b00000001
        (select field_next
         (select field_next
          (select field_next
           (select field_next (select field_next (as nil Bitvector8)))))))
       (>> #b00000001
        (select field_next
         (select field_next
          (select field_next
           (select field_next
            (select field_next (select field_next (as nil Bitvector8))))))))))
     #b00000000)
    (= footprint0
     (bit-or (>> #b00000001 (as nil Bitvector8))
      (>> #b00000001 (select field_next (as nil Bitvector8)))
      (>> #b00000001
       (select field_next (select field_next (as nil Bitvector8))))
      (>> #b00000001
       (select field_next
        (select field_next (select field_next (as nil Bitvector8)))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bitvector8))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8)))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (select field_next (as nil Bitvector8))))))))))
    (= (>> #b00000001 (as nil Bitvector8))
     (>> #b00000001 (as nil Bitvector8)))
    (=
     (bit-and (>> #b00000001 (select field_next (as nil Bitvector8)))
      (bit-or
       (>> #b00000001
        (select field_next (select field_next (as nil Bitvector8))))
       (>> #b00000001
        (select field_next
         (select field_next (select field_next (as nil Bitvector8)))))
       (>> #b00000001
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8))))))
       (>> #b00000001
        (select field_next
         (select field_next
          (select field_next
           (select field_next (select field_next (as nil Bitvector8)))))))
       (>> #b00000001
        (select field_next
         (select field_next
          (select field_next
           (select field_next
            (select field_next (select field_next (as nil Bitvector8))))))))))
     #b00000000)
    (=
     (bit-or (>> #b00000001 (select field_next (as nil Bitvector8)))
      (>> #b00000001
       (select field_next (select field_next (as nil Bitvector8))))
      (>> #b00000001
       (select field_next
        (select field_next (select field_next (as nil Bitvector8)))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bitvector8))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8)))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (select field_next (as nil Bitvector8)))))))))
     (bit-or (>> #b00000001 (select field_next (as nil Bitvector8)))
      (>> #b00000001
       (select field_next (select field_next (as nil Bitvector8))))
      (>> #b00000001
       (select field_next
        (select field_next (select field_next (as nil Bitvector8)))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bitvector8))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8)))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (select field_next (as nil Bitvector8))))))))))
    (= (>> #b00000001 (select field_next (as nil Bitvector8)))
     (>> #b00000001 (select field_next (as nil Bitvector8))))
    (=
     (bit-and
      (>> #b00000001
       (select field_next (select field_next (as nil Bitvector8))))
      (bit-or
       (>> #b00000001
        (select field_next
         (select field_next (select field_next (as nil Bitvector8)))))
       (>> #b00000001
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8))))))
       (>> #b00000001
        (select field_next
         (select field_next
          (select field_next
           (select field_next (select field_next (as nil Bitvector8)))))))
       (>> #b00000001
        (select field_next
         (select field_next
          (select field_next
           (select field_next
            (select field_next (select field_next (as nil Bitvector8))))))))))
     #b00000000)
    (=
     (bit-or
      (>> #b00000001
       (select field_next (select field_next (as nil Bitvector8))))
      (>> #b00000001
       (select field_next
        (select field_next (select field_next (as nil Bitvector8)))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bitvector8))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8)))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (select field_next (as nil Bitvector8)))))))))
     (bit-or
      (>> #b00000001
       (select field_next (select field_next (as nil Bitvector8))))
      (>> #b00000001
       (select field_next
        (select field_next (select field_next (as nil Bitvector8)))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bitvector8))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8)))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (select field_next (as nil Bitvector8))))))))))
    (=
     (>> #b00000001
      (select field_next (select field_next (as nil Bitvector8))))
     (>> #b00000001
      (select field_next (select field_next (as nil Bitvector8)))))
    (=
     (bit-and
      (>> #b00000001
       (select field_next
        (select field_next (select field_next (as nil Bitvector8)))))
      (bit-or
       (>> #b00000001
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8))))))
       (>> #b00000001
        (select field_next
         (select field_next
          (select field_next
           (select field_next (select field_next (as nil Bitvector8)))))))
       (>> #b00000001
        (select field_next
         (select field_next
          (select field_next
           (select field_next
            (select field_next (select field_next (as nil Bitvector8))))))))))
     #b00000000)
    (=
     (bit-or
      (>> #b00000001
       (select field_next
        (select field_next (select field_next (as nil Bitvector8)))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bitvector8))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8)))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (select field_next (as nil Bitvector8)))))))))
     (bit-or
      (>> #b00000001
       (select field_next
        (select field_next (select field_next (as nil Bitvector8)))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bitvector8))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8)))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (select field_next (as nil Bitvector8))))))))))
    (=
     (>> #b00000001
      (select field_next
       (select field_next (select field_next (as nil Bitvector8)))))
     (>> #b00000001
      (select field_next
       (select field_next (select field_next (as nil Bitvector8))))))
    (=
     (bit-and
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bitvector8))))))
      (bit-or
       (>> #b00000001
        (select field_next
         (select field_next
          (select field_next
           (select field_next (select field_next (as nil Bitvector8)))))))
       (>> #b00000001
        (select field_next
         (select field_next
          (select field_next
           (select field_next
            (select field_next (select field_next (as nil Bitvector8))))))))))
     #b00000000)
    (=
     (bit-or
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bitvector8))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8)))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (select field_next (as nil Bitvector8)))))))))
     (bit-or
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bitvector8))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8)))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (select field_next (as nil Bitvector8))))))))))
    (=
     (>> #b00000001
      (select field_next
       (select field_next
        (select field_next (select field_next (as nil Bitvector8))))))
     (>> #b00000001
      (select field_next
       (select field_next
        (select field_next (select field_next (as nil Bitvector8)))))))
    (=
     (bit-and
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8)))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (select field_next (as nil Bitvector8)))))))))
     #b00000000)
    (=
     (bit-or
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8)))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (select field_next (as nil Bitvector8)))))))))
     (bit-or
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8)))))))
      (>> #b00000001
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (select field_next (as nil Bitvector8))))))))))
    (=
     (>> #b00000001
      (select field_next
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bitvector8)))))))
     (>> #b00000001
      (select field_next
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bitvector8))))))))
    true
    (=
     (>> #b00000001
      (select field_next
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8))))))))
     (>> #b00000001
      (select field_next
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8)))))))))
    (=
     (>> #b00000001
      (select field_next
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8))))))))
     (>> #b00000001
      (select field_next
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8)))))))))
    (=
     (select field_next
      (select field_next
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bitvector8))))))))
     x)
    (= #b00000000 #b00000000)))
  (not (bit-check footprint0 (as nil Bool)))
  (= #b00000000 (as nil Bitvector8)) (bit-check Ref_LS (select field_next x))
  (bit-check Ref_LS (select field_next (select field_next x)))
  (bit-check Ref_LS y) (bit-check Ref_LS (select field_next y))
  (bit-check Ref_LS (select field_next (select field_next y)))
  (bit-check Ref_LS z) (bit-check Ref_LS (select field_next (as nil Bool)))
  (bit-check Ref_LS (select field_next (select field_next (as nil Bool))))
  (bit-check Ref_LS
   (select field_next (select field_next (select field_next (as nil Bool)))))
  (bit-check Ref_LS
   (select field_next
    (select field_next (select field_next (select field_next (as nil Bool))))))
  (bit-check Ref_LS
   (select field_next
    (select field_next
     (select field_next
      (select field_next (select field_next (as nil Bool)))))))
  (bit-check Ref_LS
   (select field_next
    (select field_next
     (select field_next
      (select field_next
       (select field_next (select field_next (as nil Bool))))))))
  (bit-check Ref_LS
   (select field_next
    (select field_next
     (select field_next
      (select field_next
       (select field_next
        (select field_next (select field_next (as nil Bool)))))))))
  (bit-check Ref_LS x)
  (= Ref_LS
   (bit-or (>> #b00000001 #b00000000) (>> #b00000001 #b00000001)
    (>> #b00000001 #b00000010) (>> #b00000001 #b00000011)
    (>> #b00000001 #b00000100) (>> #b00000001 #b00000101)
    (>> #b00000001 #b00000110) (>> #b00000001 #b00000111)))
  (= #b00000000 (select field_next #b00000000))))