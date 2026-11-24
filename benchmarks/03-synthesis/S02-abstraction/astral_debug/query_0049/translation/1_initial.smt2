(set-info :status unknown)

(declare-const footprint2 (Set (_ BitVec 8)))
(declare-const footprint3 (Set (_ BitVec 8)))
(declare-const footprint4 (Set (_ BitVec 8)))
(declare-const footprint5 (Set (_ BitVec 8)))
(declare-const footprint6 (Set (_ BitVec 8)))
(declare-const footprint7 (Set (_ BitVec 8)))
(declare-const footprint8 (Set (_ BitVec 8)))
(declare-const footprint9 (Set (_ BitVec 8)))
(declare-const footprint10 (Set (_ BitVec 8)))
(declare-const footprint11 (Set (_ BitVec 8)))
(declare-const footprint12 (Set (_ BitVec 8)))
(declare-const footprint13 (Set (_ BitVec 8)))
(declare-const footprint14 (Set (_ BitVec 8)))
(declare-const footprint15 (Set (_ BitVec 8)))
(declare-const footprint16 (Set (_ BitVec 8)))
(declare-const footprint17 (Set (_ BitVec 8)))
(declare-const footprint18 (Set (_ BitVec 8)))
(declare-const footprint19 (Set (_ BitVec 8)))
(declare-const footprint20 (Set (_ BitVec 8)))
(declare-const footprint21 (Set (_ BitVec 8)))
(declare-const footprint22 (Set (_ BitVec 8)))
(declare-const footprint23 (Set (_ BitVec 8)))
(declare-const footprint24 (Set (_ BitVec 8)))
(declare-const footprint25 (Set (_ BitVec 8)))
(declare-const footprint26 (Set (_ BitVec 8)))
(declare-const footprint27 (Set (_ BitVec 8)))
(declare-const footprint28 (Set (_ BitVec 8)))
(declare-const footprint29 (Set (_ BitVec 8)))
(declare-const footprint30 (Set (_ BitVec 8)))
(declare-const footprint31 (Set (_ BitVec 8)))
(declare-const footprint0 (Set (_ BitVec 8)))
(declare-const nil (_ BitVec 8))
(declare-const y (_ BitVec 8))
(declare-const x (_ BitVec 8))
(declare-const z (_ BitVec 8))
(declare-const Ref_LS (Set (_ BitVec 8)))
(declare-const field_next (Array (_ BitVec 8) (_ BitVec 8)))

(assert
 (and (distinct x (select field_next x)) (= footprint2 set)
  (distinct x (select field_next (select field_next x))) (= footprint3 set)
  (distinct x y) (= footprint4 set) (distinct x (select field_next y))
  (= footprint5 set) (distinct x (select field_next (select field_next y)))
  (= footprint6 set)
  (distinct (select field_next x) (select field_next (select field_next x)))
  (= footprint7 set) (distinct (select field_next x) y) (= footprint8 set)
  (distinct (select field_next x) (select field_next y)) (= footprint9 set)
  (distinct (select field_next x) (select field_next (select field_next y)))
  (= footprint10 set) (distinct (select field_next (select field_next x)) y)
  (= footprint11 set)
  (distinct (select field_next (select field_next x)) (select field_next y))
  (= footprint12 set)
  (distinct (select field_next (select field_next x))
   (select field_next (select field_next y)))
  (= footprint13 set) (distinct y (select field_next y)) (= footprint14 set)
  (distinct y (select field_next (select field_next y))) (= footprint15 set)
  (distinct (select field_next y) (select field_next (select field_next y)))
  (= footprint16 set) (distinct x (select field_next x)) (= footprint17 set)
  (distinct x (select field_next (select field_next x))) (= footprint18 set)
  (distinct x y) (= footprint19 set)
  (distinct (select field_next x) (select field_next (select field_next x)))
  (= footprint20 set) (distinct (select field_next x) y) (= footprint21 set)
  (distinct (select field_next (select field_next x)) y) (= footprint22 set)
  (distinct y (select field_next y)) (= footprint23 set)
  (distinct y (select field_next (select field_next y))) (= footprint24 set)
  (distinct y z) (= footprint25 set)
  (distinct (select field_next y) (select field_next (select field_next y)))
  (= footprint26 set) (distinct (select field_next y) z) (= footprint27 set)
  (distinct (select field_next (select field_next y)) z) (= footprint28 set)
  (= footprint29 (set y)) (= footprint30 (set (select field_next y)))
  (= footprint31 (set (select field_next (select field_next y))))
  (= (select field_next (select field_next (select field_next y))) z)
  (disjoint footprint2 footprint3 footprint4 footprint5 footprint6 footprint7
   footprint8 footprint9 footprint10 footprint11 footprint12 footprint13
   footprint14 footprint15 footprint16 footprint17 footprint18 footprint19
   footprint20 footprint21 footprint22 footprint23 footprint24 footprint25
   footprint26 footprint27 footprint28 footprint29 footprint30 footprint31)
  (= footprint0
   (union footprint2 footprint3 footprint4 footprint5 footprint6 footprint7
    footprint8 footprint9 footprint10 footprint11 footprint12 footprint13
    footprint14 footprint15 footprint16 footprint17 footprint18 footprint19
    footprint20 footprint21 footprint22 footprint23 footprint24 footprint25
    footprint26 footprint27 footprint28 footprint29 footprint30 footprint31))
  (not
   (ite (= x z) (= footprint0 set)
    (and
     (disjoint (set x)
      (ite (= (select field_next x) z) set
       (union (set (select field_next x))
        (ite (= (select field_next (select field_next x)) z) set
         (union (set (select field_next (select field_next x)))
          (ite
           (= (select field_next (select field_next (select field_next x)))
            z)
           set
           (union
            (set
             (select field_next (select field_next (select field_next x))))
            (ite
             (=
              (select field_next
               (select field_next (select field_next (select field_next x))))
              z)
             set
             (union
              (set
               (select field_next
                (select field_next (select field_next (select field_next x)))))
              (ite
               (=
                (select field_next
                 (select field_next
                  (select field_next
                   (select field_next (select field_next x)))))
                z)
               set
               (union
                (set
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x))))))
                (ite
                 (=
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next (select field_next x))))))
                  z)
                 set
                 (set
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next (select field_next x)))))))))))))))))))
     (= footprint0
      (union (set x)
       (ite (= (select field_next x) z) set
        (union (set (select field_next x))
         (ite (= (select field_next (select field_next x)) z) set
          (union (set (select field_next (select field_next x)))
           (ite
            (= (select field_next (select field_next (select field_next x)))
             z)
            set
            (union
             (set
              (select field_next (select field_next (select field_next x))))
             (ite
              (=
               (select field_next
                (select field_next (select field_next (select field_next x))))
               z)
              set
              (union
               (set
                (select field_next
                 (select field_next
                  (select field_next (select field_next x)))))
               (ite
                (=
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x)))))
                 z)
                set
                (union
                 (set
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next (select field_next x))))))
                 (ite
                  (=
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))
                   z)
                  set
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))))))))))))))))
     (= (set x) (set x))
     (ite (= (select field_next x) z)
      (=
       (ite (= (select field_next x) z) set
        (union (set (select field_next x))
         (ite (= (select field_next (select field_next x)) z) set
          (union (set (select field_next (select field_next x)))
           (ite
            (= (select field_next (select field_next (select field_next x)))
             z)
            set
            (union
             (set
              (select field_next (select field_next (select field_next x))))
             (ite
              (=
               (select field_next
                (select field_next (select field_next (select field_next x))))
               z)
              set
              (union
               (set
                (select field_next
                 (select field_next
                  (select field_next (select field_next x)))))
               (ite
                (=
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x)))))
                 z)
                set
                (union
                 (set
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next (select field_next x))))))
                 (ite
                  (=
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))
                   z)
                  set
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))))))))))))))
       set)
      (and
       (disjoint (set (select field_next x))
        (ite (= (select field_next (select field_next x)) z) set
         (union (set (select field_next (select field_next x)))
          (ite
           (= (select field_next (select field_next (select field_next x)))
            z)
           set
           (union
            (set
             (select field_next (select field_next (select field_next x))))
            (ite
             (=
              (select field_next
               (select field_next (select field_next (select field_next x))))
              z)
             set
             (union
              (set
               (select field_next
                (select field_next (select field_next (select field_next x)))))
              (ite
               (=
                (select field_next
                 (select field_next
                  (select field_next
                   (select field_next (select field_next x)))))
                z)
               set
               (union
                (set
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x))))))
                (ite
                 (=
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next (select field_next x))))))
                  z)
                 set
                 (set
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next (select field_next x)))))))))))))))))
       (=
        (ite (= (select field_next x) z) set
         (union (set (select field_next x))
          (ite (= (select field_next (select field_next x)) z) set
           (union (set (select field_next (select field_next x)))
            (ite
             (= (select field_next (select field_next (select field_next x)))
              z)
             set
             (union
              (set
               (select field_next (select field_next (select field_next x))))
              (ite
               (=
                (select field_next
                 (select field_next
                  (select field_next (select field_next x))))
                z)
               set
               (union
                (set
                 (select field_next
                  (select field_next
                   (select field_next (select field_next x)))))
                (ite
                 (=
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next (select field_next x)))))
                  z)
                 set
                 (union
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next (select field_next x))))))
                  (ite
                   (=
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next
                        (select field_next (select field_next x))))))
                    z)
                   set
                   (set
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next
                        (select field_next (select field_next x))))))))))))))))))
        (union (set (select field_next x))
         (ite (= (select field_next (select field_next x)) z) set
          (union (set (select field_next (select field_next x)))
           (ite
            (= (select field_next (select field_next (select field_next x)))
             z)
            set
            (union
             (set
              (select field_next (select field_next (select field_next x))))
             (ite
              (=
               (select field_next
                (select field_next (select field_next (select field_next x))))
               z)
              set
              (union
               (set
                (select field_next
                 (select field_next
                  (select field_next (select field_next x)))))
               (ite
                (=
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x)))))
                 z)
                set
                (union
                 (set
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next (select field_next x))))))
                 (ite
                  (=
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))
                   z)
                  set
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))))))))))))))
       (= (set (select field_next x)) (set (select field_next x)))
       (ite (= (select field_next (select field_next x)) z)
        (=
         (ite (= (select field_next (select field_next x)) z) set
          (union (set (select field_next (select field_next x)))
           (ite
            (= (select field_next (select field_next (select field_next x)))
             z)
            set
            (union
             (set
              (select field_next (select field_next (select field_next x))))
             (ite
              (=
               (select field_next
                (select field_next (select field_next (select field_next x))))
               z)
              set
              (union
               (set
                (select field_next
                 (select field_next
                  (select field_next (select field_next x)))))
               (ite
                (=
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x)))))
                 z)
                set
                (union
                 (set
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next (select field_next x))))))
                 (ite
                  (=
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))
                   z)
                  set
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))))))))))))
         set)
        (and
         (disjoint (set (select field_next (select field_next x)))
          (ite
           (= (select field_next (select field_next (select field_next x)))
            z)
           set
           (union
            (set
             (select field_next (select field_next (select field_next x))))
            (ite
             (=
              (select field_next
               (select field_next (select field_next (select field_next x))))
              z)
             set
             (union
              (set
               (select field_next
                (select field_next (select field_next (select field_next x)))))
              (ite
               (=
                (select field_next
                 (select field_next
                  (select field_next
                   (select field_next (select field_next x)))))
                z)
               set
               (union
                (set
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x))))))
                (ite
                 (=
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next (select field_next x))))))
                  z)
                 set
                 (set
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next (select field_next x)))))))))))))))
         (=
          (ite (= (select field_next (select field_next x)) z) set
           (union (set (select field_next (select field_next x)))
            (ite
             (= (select field_next (select field_next (select field_next x)))
              z)
             set
             (union
              (set
               (select field_next (select field_next (select field_next x))))
              (ite
               (=
                (select field_next
                 (select field_next
                  (select field_next (select field_next x))))
                z)
               set
               (union
                (set
                 (select field_next
                  (select field_next
                   (select field_next (select field_next x)))))
                (ite
                 (=
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next (select field_next x)))))
                  z)
                 set
                 (union
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next (select field_next x))))))
                  (ite
                   (=
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next
                        (select field_next (select field_next x))))))
                    z)
                   set
                   (set
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next
                        (select field_next (select field_next x))))))))))))))))
          (union (set (select field_next (select field_next x)))
           (ite
            (= (select field_next (select field_next (select field_next x)))
             z)
            set
            (union
             (set
              (select field_next (select field_next (select field_next x))))
             (ite
              (=
               (select field_next
                (select field_next (select field_next (select field_next x))))
               z)
              set
              (union
               (set
                (select field_next
                 (select field_next
                  (select field_next (select field_next x)))))
               (ite
                (=
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x)))))
                 z)
                set
                (union
                 (set
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next (select field_next x))))))
                 (ite
                  (=
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))
                   z)
                  set
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))))))))))))
         (= (set (select field_next (select field_next x)))
          (set (select field_next (select field_next x))))
         (ite
          (= (select field_next (select field_next (select field_next x))) z)
          (=
           (ite
            (= (select field_next (select field_next (select field_next x)))
             z)
            set
            (union
             (set
              (select field_next (select field_next (select field_next x))))
             (ite
              (=
               (select field_next
                (select field_next (select field_next (select field_next x))))
               z)
              set
              (union
               (set
                (select field_next
                 (select field_next
                  (select field_next (select field_next x)))))
               (ite
                (=
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x)))))
                 z)
                set
                (union
                 (set
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next (select field_next x))))))
                 (ite
                  (=
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))
                   z)
                  set
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))))))))))
           set)
          (and
           (disjoint
            (set
             (select field_next (select field_next (select field_next x))))
            (ite
             (=
              (select field_next
               (select field_next (select field_next (select field_next x))))
              z)
             set
             (union
              (set
               (select field_next
                (select field_next (select field_next (select field_next x)))))
              (ite
               (=
                (select field_next
                 (select field_next
                  (select field_next
                   (select field_next (select field_next x)))))
                z)
               set
               (union
                (set
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x))))))
                (ite
                 (=
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next (select field_next x))))))
                  z)
                 set
                 (set
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next (select field_next x)))))))))))))
           (=
            (ite
             (= (select field_next (select field_next (select field_next x)))
              z)
             set
             (union
              (set
               (select field_next (select field_next (select field_next x))))
              (ite
               (=
                (select field_next
                 (select field_next
                  (select field_next (select field_next x))))
                z)
               set
               (union
                (set
                 (select field_next
                  (select field_next
                   (select field_next (select field_next x)))))
                (ite
                 (=
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next (select field_next x)))))
                  z)
                 set
                 (union
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next (select field_next x))))))
                  (ite
                   (=
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next
                        (select field_next (select field_next x))))))
                    z)
                   set
                   (set
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next
                        (select field_next (select field_next x))))))))))))))
            (union
             (set
              (select field_next (select field_next (select field_next x))))
             (ite
              (=
               (select field_next
                (select field_next (select field_next (select field_next x))))
               z)
              set
              (union
               (set
                (select field_next
                 (select field_next
                  (select field_next (select field_next x)))))
               (ite
                (=
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x)))))
                 z)
                set
                (union
                 (set
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next (select field_next x))))))
                 (ite
                  (=
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))
                   z)
                  set
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))))))))))
           (=
            (set
             (select field_next (select field_next (select field_next x))))
            (set
             (select field_next (select field_next (select field_next x)))))
           (ite
            (=
             (select field_next
              (select field_next (select field_next (select field_next x))))
             z)
            (=
             (ite
              (=
               (select field_next
                (select field_next (select field_next (select field_next x))))
               z)
              set
              (union
               (set
                (select field_next
                 (select field_next
                  (select field_next (select field_next x)))))
               (ite
                (=
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x)))))
                 z)
                set
                (union
                 (set
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next (select field_next x))))))
                 (ite
                  (=
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))
                   z)
                  set
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))))))))
             set)
            (and
             (disjoint
              (set
               (select field_next
                (select field_next (select field_next (select field_next x)))))
              (ite
               (=
                (select field_next
                 (select field_next
                  (select field_next
                   (select field_next (select field_next x)))))
                z)
               set
               (union
                (set
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x))))))
                (ite
                 (=
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next (select field_next x))))))
                  z)
                 set
                 (set
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next (select field_next x)))))))))))
             (=
              (ite
               (=
                (select field_next
                 (select field_next
                  (select field_next (select field_next x))))
                z)
               set
               (union
                (set
                 (select field_next
                  (select field_next
                   (select field_next (select field_next x)))))
                (ite
                 (=
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next (select field_next x)))))
                  z)
                 set
                 (union
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next (select field_next x))))))
                  (ite
                   (=
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next
                        (select field_next (select field_next x))))))
                    z)
                   set
                   (set
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next
                        (select field_next (select field_next x))))))))))))
              (union
               (set
                (select field_next
                 (select field_next
                  (select field_next (select field_next x)))))
               (ite
                (=
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x)))))
                 z)
                set
                (union
                 (set
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next (select field_next x))))))
                 (ite
                  (=
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))
                   z)
                  set
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))))))))
             (=
              (set
               (select field_next
                (select field_next (select field_next (select field_next x)))))
              (set
               (select field_next
                (select field_next (select field_next (select field_next x))))))
             (ite
              (=
               (select field_next
                (select field_next
                 (select field_next
                  (select field_next (select field_next x)))))
               z)
              (=
               (ite
                (=
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x)))))
                 z)
                set
                (union
                 (set
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next (select field_next x))))))
                 (ite
                  (=
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))
                   z)
                  set
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))))))
               set)
              (and
               (disjoint
                (set
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x))))))
                (ite
                 (=
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next (select field_next x))))))
                  z)
                 set
                 (set
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next (select field_next x)))))))))
               (=
                (ite
                 (=
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next (select field_next x)))))
                  z)
                 set
                 (union
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next (select field_next x))))))
                  (ite
                   (=
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next
                        (select field_next (select field_next x))))))
                    z)
                   set
                   (set
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next
                        (select field_next (select field_next x))))))))))
                (union
                 (set
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next (select field_next x))))))
                 (ite
                  (=
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))
                   z)
                  set
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))))))
               (=
                (set
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x))))))
                (set
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next (select field_next x)))))))
               (ite
                (=
                 (select field_next
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next (select field_next x))))))
                 z)
                (=
                 (ite
                  (=
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))
                   z)
                  set
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))))
                 set)
                (and
                 (disjoint
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x)))))))
                  set)
                 (=
                  (ite
                   (=
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next
                        (select field_next (select field_next x))))))
                    z)
                   set
                   (set
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next
                        (select field_next (select field_next x))))))))
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))))
                 (=
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x)))))))
                  (set
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x))))))))
                 (=
                  (select field_next
                   (select field_next
                    (select field_next
                     (select field_next
                      (select field_next
                       (select field_next (select field_next x)))))))
                  z)
                 (= set set))))))))))))))))
  (not (mem (as nil Bool) footprint0)) (= #b00000000 (as nil Bitvector8))
  (mem y Ref_LS) (mem (select field_next y) Ref_LS)
  (mem (select field_next (select field_next y)) Ref_LS) (mem x Ref_LS)
  (mem (select field_next x) Ref_LS)
  (mem (select field_next (select field_next x)) Ref_LS)
  (mem (select field_next (select field_next (select field_next x))) Ref_LS)
  (mem
   (select field_next
    (select field_next (select field_next (select field_next x))))
   Ref_LS)
  (mem
   (select field_next
    (select field_next
     (select field_next (select field_next (select field_next x)))))
   Ref_LS)
  (mem
   (select field_next
    (select field_next
     (select field_next
      (select field_next (select field_next (select field_next x))))))
   Ref_LS)
  (mem
   (select field_next
    (select field_next
     (select field_next
      (select field_next
       (select field_next (select field_next (select field_next x)))))))
   Ref_LS)
  (mem z Ref_LS)
  (= Ref_LS
   (set #b00000000 #b00000001 #b00000010 #b00000011 #b00000100 #b00000101
    #b00000110 #b00000111))
  (= #b00000000 (select field_next #b00000000))))