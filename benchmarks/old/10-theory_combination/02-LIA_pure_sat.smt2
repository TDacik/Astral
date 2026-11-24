(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Int)

(assert (= (- x 1) 42))

(check-sat)
