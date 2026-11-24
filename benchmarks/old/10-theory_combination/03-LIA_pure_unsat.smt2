(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x Int)

(assert (= (* x 0) 1))

(check-sat)
