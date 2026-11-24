(set-info :status unknown)

(declare-const footprint31 (Set (_ BitVec 7)))
(declare-const footprint32 (Set (_ BitVec 7)))
(declare-const footprint29 (Set (_ BitVec 7)))
(declare-const footprint30 (Set (_ BitVec 7)))
(declare-const footprint27 (Set (_ BitVec 7)))
(declare-const footprint28 (Set (_ BitVec 7)))
(declare-const footprint2 (Set (_ BitVec 7)))
(declare-const footprint3 (Set (_ BitVec 7)))
(declare-const footprint4 (Set (_ BitVec 7)))
(declare-const footprint5 (Set (_ BitVec 7)))
(declare-const footprint6 (Set (_ BitVec 7)))
(declare-const footprint7 (Set (_ BitVec 7)))
(declare-const footprint8 (Set (_ BitVec 7)))
(declare-const footprint9 (Set (_ BitVec 7)))
(declare-const footprint10 (Set (_ BitVec 7)))
(declare-const footprint11 (Set (_ BitVec 7)))
(declare-const footprint12 (Set (_ BitVec 7)))
(declare-const footprint13 (Set (_ BitVec 7)))
(declare-const footprint14 (Set (_ BitVec 7)))
(declare-const footprint15 (Set (_ BitVec 7)))
(declare-const footprint16 (Set (_ BitVec 7)))
(declare-const footprint17 (Set (_ BitVec 7)))
(declare-const footprint18 (Set (_ BitVec 7)))
(declare-const footprint19 (Set (_ BitVec 7)))
(declare-const footprint20 (Set (_ BitVec 7)))
(declare-const footprint21 (Set (_ BitVec 7)))
(declare-const footprint22 (Set (_ BitVec 7)))
(declare-const footprint23 (Set (_ BitVec 7)))
(declare-const footprint24 (Set (_ BitVec 7)))
(declare-const footprint25 (Set (_ BitVec 7)))
(declare-const footprint26 (Set (_ BitVec 7)))
(declare-const footprint0 (Set (_ BitVec 7)))
(declare-const y (_ BitVec 7))
(declare-const x (_ BitVec 7))
(declare-const nil (_ BitVec 7))
(declare-const z (_ BitVec 7))
(declare-const Ref_LS (Set (_ BitVec 7)))
(declare-const Ref_NLS (Set (_ BitVec 7)))
(declare-const field_down (Array (_ BitVec 7) (_ BitVec 7)))
(declare-const field_top (Array (_ BitVec 7) (_ BitVec 7)))
(declare-const field_next (Array (_ BitVec 7) (_ BitVec 7)))

(assert
 (and (distinct (as nil Bitvector7) x) (= footprint2 set)
  (distinct (as nil Bitvector7) (select field_top x)) (= footprint3 set)
  (distinct (as nil Bitvector7) y) (= footprint4 set)
  (distinct (as nil Bitvector7) (select field_down x)) (= footprint5 set)
  (distinct (as nil Bitvector7) (select field_down (select field_top x)))
  (= footprint6 set) (distinct (as nil Bitvector7) z) (= footprint7 set)
  (distinct x (select field_top x)) (= footprint8 set) (distinct x y)
  (= footprint9 set) (distinct (select field_top x) y) (= footprint10 set)
  (distinct (select field_down x) (select field_down (select field_top x)))
  (= footprint11 set) (distinct (select field_down x) z) (= footprint12 set)
  (distinct (select field_down (select field_top x)) z) (= footprint13 set)
  (distinct x (select field_top x)) (= footprint14 set) (distinct x y)
  (= footprint15 set) (distinct (select field_top x) y) (= footprint16 set)
  (distinct (select field_down x) z) (= footprint17 set)
  (distinct (select field_down x) (as nil Bitvector7)) (= footprint18 set)
  (distinct z (as nil Bitvector7)) (= footprint19 set)
  (distinct (select field_down (select field_top x)) z) (= footprint20 set)
  (distinct (select field_down (select field_top x)) (as nil Bitvector7))
  (= footprint21 set) (distinct z (as nil Bitvector7)) (= footprint22 set)
  (= footprint23 (set x)) (= footprint24 (set (select field_top x)))
  (= (select field_top (select field_top x)) y)
  (= footprint25 (set (select field_down x)))
  (= (select field_next (select field_down x)) z)
  (ite (= (select field_down (select field_top x)) z) (= footprint26 set)
   (and (= footprint27 (set (select field_down (select field_top x))))
    (ite (= (select field_next (select field_down (select field_top x))) z)
     (= footprint28 set)
     (and
      (= footprint29
       (set (select field_next (select field_down (select field_top x)))))
      (ite
       (=
        (select field_next
         (select field_next (select field_down (select field_top x))))
        z)
       (= footprint30 set)
       (and
        (= footprint31
         (set
          (select field_next
           (select field_next (select field_down (select field_top x))))))
        (=
         (select field_next
          (select field_next
           (select field_next (select field_down (select field_top x)))))
         z)
        (= footprint32 set) (disjoint footprint31 footprint32)
        (= footprint30 (union footprint31 footprint32))))
      (disjoint footprint29 footprint30)
      (= footprint28 (union footprint29 footprint30))))
    (disjoint footprint27 footprint28)
    (= footprint26 (union footprint27 footprint28))))
  (disjoint footprint2 footprint3 footprint4 footprint5 footprint6 footprint7
   footprint8 footprint9 footprint10 footprint11 footprint12 footprint13
   footprint14 footprint15 footprint16 footprint17 footprint18 footprint19
   footprint20 footprint21 footprint22 footprint23 footprint24 footprint25
   footprint26)
  (= footprint0
   (union footprint2 footprint3 footprint4 footprint5 footprint6 footprint7
    footprint8 footprint9 footprint10 footprint11 footprint12 footprint13
    footprint14 footprint15 footprint16 footprint17 footprint18 footprint19
    footprint20 footprint21 footprint22 footprint23 footprint24 footprint25
    footprint26))
  (not
   (and
    (disjoint (set (as nil Bool))
     (union (set (select field_next (as nil Bool)))
      (set (select field_next (select field_next (as nil Bool))))
      (set
       (select field_next
        (select field_next (select field_next (as nil Bool)))))
      (set
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bool))))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bool)))))))))
    (= footprint0
     (union (set (as nil "(Set (Bitvector 7))"))
      (set (select field_next (as nil "(Set (Bitvector 7))")))
      (set
       (select field_next (select field_next (as nil "(Set (Bitvector 7))"))))
      (set
       (select field_next
        (select field_next
         (select field_next (as nil "(Set (Bitvector 7))")))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next (as nil "(Set (Bitvector 7))"))))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (as nil "(Set (Bitvector 7))")))))))))
    (= (set (as nil "(Set (Bitvector 7))"))
     (set (as nil "(Set (Bitvector 7))")))
    (disjoint (set (select field_next (as nil Bool)))
     (union (set (select field_next (select field_next (as nil Bool))))
      (set
       (select field_next
        (select field_next (select field_next (as nil Bool)))))
      (set
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bool))))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bool)))))))))
    (=
     (union (set (select field_next (as nil "(Set (Bitvector 7))")))
      (set
       (select field_next (select field_next (as nil "(Set (Bitvector 7))"))))
      (set
       (select field_next
        (select field_next
         (select field_next (as nil "(Set (Bitvector 7))")))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next (as nil "(Set (Bitvector 7))"))))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (as nil "(Set (Bitvector 7))"))))))))
     (union (set (select field_next (as nil "(Set (Bitvector 7))")))
      (set
       (select field_next (select field_next (as nil "(Set (Bitvector 7))"))))
      (set
       (select field_next
        (select field_next
         (select field_next (as nil "(Set (Bitvector 7))")))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next (as nil "(Set (Bitvector 7))"))))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (as nil "(Set (Bitvector 7))")))))))))
    (= (set (select field_next (as nil "(Set (Bitvector 7))")))
     (set (select field_next (as nil "(Set (Bitvector 7))"))))
    (disjoint (set (select field_next (select field_next (as nil Bool))))
     (union
      (set
       (select field_next
        (select field_next (select field_next (as nil Bool)))))
      (set
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bool))))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bool)))))))))
    (=
     (union
      (set
       (select field_next (select field_next (as nil "(Set (Bitvector 7))"))))
      (set
       (select field_next
        (select field_next
         (select field_next (as nil "(Set (Bitvector 7))")))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next (as nil "(Set (Bitvector 7))"))))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (as nil "(Set (Bitvector 7))"))))))))
     (union
      (set
       (select field_next (select field_next (as nil "(Set (Bitvector 7))"))))
      (set
       (select field_next
        (select field_next
         (select field_next (as nil "(Set (Bitvector 7))")))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next (as nil "(Set (Bitvector 7))"))))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (as nil "(Set (Bitvector 7))")))))))))
    (=
     (set
      (select field_next (select field_next (as nil "(Set (Bitvector 7))"))))
     (set
      (select field_next (select field_next (as nil "(Set (Bitvector 7))")))))
    (disjoint
     (set
      (select field_next
       (select field_next (select field_next (as nil Bool)))))
     (union
      (set
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bool))))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next (select field_next (as nil Bool)))))))))
    (=
     (union
      (set
       (select field_next
        (select field_next
         (select field_next (as nil "(Set (Bitvector 7))")))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next (as nil "(Set (Bitvector 7))"))))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (as nil "(Set (Bitvector 7))"))))))))
     (union
      (set
       (select field_next
        (select field_next
         (select field_next (as nil "(Set (Bitvector 7))")))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next (as nil "(Set (Bitvector 7))"))))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (as nil "(Set (Bitvector 7))")))))))))
    (=
     (set
      (select field_next
       (select field_next (select field_next (as nil "(Set (Bitvector 7))")))))
     (set
      (select field_next
       (select field_next (select field_next (as nil "(Set (Bitvector 7))"))))))
    (disjoint
     (set
      (select field_next
       (select field_next
        (select field_next (select field_next (as nil Bool))))))
     (set
      (select field_next
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bool))))))))
    (=
     (union
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next (as nil "(Set (Bitvector 7))"))))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (as nil "(Set (Bitvector 7))"))))))))
     (union
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next (as nil "(Set (Bitvector 7))"))))))
      (set
       (select field_next
        (select field_next
         (select field_next
          (select field_next
           (select field_next (as nil "(Set (Bitvector 7))")))))))))
    (=
     (set
      (select field_next
       (select field_next
        (select field_next
         (select field_next (as nil "(Set (Bitvector 7))"))))))
     (set
      (select field_next
       (select field_next
        (select field_next
         (select field_next (as nil "(Set (Bitvector 7))")))))))
    (disjoint
     (set
      (select field_next
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bool)))))))
     set)
    (=
     (set
      (select field_next
       (select field_next
        (select field_next
         (select field_next
          (select field_next (as nil "(Set (Bitvector 7))")))))))
     (set
      (select field_next
       (select field_next
        (select field_next
         (select field_next
          (select field_next (as nil "(Set (Bitvector 7))"))))))))
    (=
     (set
      (select field_next
       (select field_next
        (select field_next
         (select field_next
          (select field_next (as nil "(Set (Bitvector 7))")))))))
     (set
      (select field_next
       (select field_next
        (select field_next
         (select field_next
          (select field_next (as nil "(Set (Bitvector 7))"))))))))
    (=
     (select field_next
      (select field_next
       (select field_next
        (select field_next
         (select field_next (select field_next (as nil Bitvector7)))))))
     z)
    (= set set)))
  (not (mem (as nil Bool) footprint0)) (= #b0000000 (as nil Bitvector7))
  (mem x Ref_NLS) (mem (select field_top x) Ref_NLS) (mem y Ref_NLS)
  (mem (select field_down x) Ref_LS)
  (mem (select field_down (select field_top x)) Ref_LS)
  (mem (select field_next (select field_down (select field_top x))) Ref_LS)
  (mem
   (select field_next
    (select field_next (select field_down (select field_top x))))
   Ref_LS)
  (mem
   (select field_next
    (select field_next
     (select field_next (select field_down (select field_top x)))))
   Ref_LS)
  (mem (select field_next (as nil Bool)) Ref_LS)
  (mem (select field_next (select field_next (as nil Bool))) Ref_LS)
  (mem
   (select field_next (select field_next (select field_next (as nil Bool))))
   Ref_LS)
  (mem
   (select field_next
    (select field_next (select field_next (select field_next (as nil Bool)))))
   Ref_LS)
  (mem
   (select field_next
    (select field_next
     (select field_next
      (select field_next (select field_next (as nil Bool))))))
   Ref_LS)
  (mem
   (select field_next
    (select field_next
     (select field_next
      (select field_next
       (select field_next (select field_next (as nil Bool)))))))
   Ref_LS)
  (mem z Ref_LS) (= Ref_LS (set #b0000000 #b0000001 #b0000010 #b0000011))
  (= Ref_NLS (set #b0000000 #b0000100 #b0000101 #b0000110))
  (= #b0000000 (select field_down #b0000000))
  (= #b0000000 (select field_top #b0000000))
  (= #b0000000 (select field_next #b0000000))))