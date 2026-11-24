(set-info :status unknown)

(declare-const z Ref_LS)
(declare-const x Ref_NLS)
(declare-const y Ref_NLS)

(assert
 (sep (distinct (as nil Ref_NLS) x) (distinct (as nil Ref_NLS) (top[.] x))
  (distinct (as nil Ref_NLS) y) (distinct (as nil Ref_LS) (down[.] x))
  (distinct (as nil Ref_LS) (down[.] (top[.] x)))
  (distinct (as nil Ref_LS) z) (distinct x (top[.] x)) (distinct x y)
  (distinct (top[.] x) y) (distinct (down[.] x) (down[.] (top[.] x)))
  (distinct (down[.] x) z) (distinct (down[.] (top[.] x)) z)
  (distinct x (top[.] x)) (distinct x y) (distinct (top[.] x) y)
  (distinct (down[.] x) z) (distinct (down[.] x) (as nil Ref_LS))
  (distinct z (as nil Ref_LS)) (distinct (down[.] (top[.] x)) z)
  (distinct (down[.] (top[.] x)) (as nil Ref_LS))
  (distinct z (as nil Ref_LS)) (pto x (c_NLS (top[.] x) (down[.] x)))
  (pto (top[.] x) (c_NLS y (down[.] (top[.] x)))) (pto (down[.] x) (c_LS z))
  (ite (= (down[.] (top[.] x)) z) emp
   (sep (pto (down[.] (top[.] x)) (c_LS (next[.] (down[.] (top[.] x)))))
    (ite (= (next[.] (down[.] (top[.] x))) z) emp
     (sep
      (pto (next[.] (down[.] (top[.] x)))
       (c_LS (next[.] (next[.] (down[.] (top[.] x))))))
      (ite (= (next[.] (next[.] (down[.] (top[.] x)))) z) emp
       (sep
        (pto (next[.] (next[.] (down[.] (top[.] x))))
         (c_LS (next[.] (next[.] (next[.] (down[.] (top[.] x)))))))
        (= (next[.] (next[.] (next[.] (down[.] (top[.] x))))) z)))))))))

(assert
 (not
  (sep (pto x (c_LS (next[.] x)))
   (ite (= (next[.] x) y) emp
    (sep (pto (next[.] x) (c_LS (next[.] (next[.] x))))
     (ite (= (next[.] (next[.] x)) y) emp
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
             (= (next[.] (next[.] (next[.] (next[.] (next[.] (next[.] x))))))
              y))))))))))))))