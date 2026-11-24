(set-info :status unknown)

(declare-const z Ref_LS)
(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert
 (sep (distinct x (next[.] x)) (distinct x (next[.] (next[.] x)))
  (distinct x y) (distinct x (next[.] y)) (distinct x (next[.] (next[.] y)))
  (distinct (next[.] x) (next[.] (next[.] x))) (distinct (next[.] x) y)
  (distinct (next[.] x) (next[.] y))
  (distinct (next[.] x) (next[.] (next[.] y)))
  (distinct (next[.] (next[.] x)) y)
  (distinct (next[.] (next[.] x)) (next[.] y))
  (distinct (next[.] (next[.] x)) (next[.] (next[.] y)))
  (distinct y (next[.] y)) (distinct y (next[.] (next[.] y)))
  (distinct (next[.] y) (next[.] (next[.] y))) (distinct x (next[.] x))
  (distinct x (next[.] (next[.] x))) (distinct x y)
  (distinct (next[.] x) (next[.] (next[.] x))) (distinct (next[.] x) y)
  (distinct (next[.] (next[.] x)) y) (distinct y (next[.] y))
  (distinct y (next[.] (next[.] y))) (distinct y z)
  (distinct (next[.] y) (next[.] (next[.] y))) (distinct (next[.] y) z)
  (distinct (next[.] (next[.] y)) z) (pto y (c_LS (next[.] y)))
  (pto (next[.] y) (c_LS (next[.] (next[.] y))))
  (pto (next[.] (next[.] y)) (c_LS z))))

(assert
 (not
  (sep (pto x (c_LS (next[.] x)))
   (sep (pto (next[.] x) (c_LS (next[.] (next[.] x))))
    (sep (pto (next[.] (next[.] x)) (c_LS (next[.] (next[.] (next[.] x)))))
     (ite (= (next[.] (next[.] (next[.] x))) y) emp
      (sep
       (pto (next[.] (next[.] (next[.] x)))
        (c_LS (next[.] (next[.] (next[.] (next[.] x))))))
       (ite (= (next[.] (next[.] (next[.] (next[.] x)))) y) emp
        (sep
         (pto (next[.] (next[.] (next[.] (next[.] x))))
          (c_LS (next[.] (next[.] (next[.] (next[.] (next[.] x)))))))
         (ite (= (next[.] (next[.] (next[.] (next[.] (next[.] x))))) y) emp
          (sep
           (pto (next[.] (next[.] (next[.] (next[.] (next[.] x)))))
            (c_LS
             (next[.] (next[.] (next[.] (next[.] (next[.] (next[.] x))))))))
           (ite
            (= (next[.] (next[.] (next[.] (next[.] (next[.] (next[.] x))))))
             y)
            emp
            (sep
             (pto
              (next[.] (next[.] (next[.] (next[.] (next[.] (next[.] x))))))
              (c_LS
               (next[.]
                (next[.] (next[.] (next[.] (next[.] (next[.] (next[.] x)))))))))
             (=
              (next[.]
               (next[.] (next[.] (next[.] (next[.] (next[.] (next[.] x)))))))
              y))))))))))))))