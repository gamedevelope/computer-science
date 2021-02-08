#lang sicp
(#%require "Figure.Interp.V2.rkt")
(eval '((lambda (n)
          ((lambda (fact)
             (fact fact n))
           (lambda (ft k)
             (if (= k 1)
                 1
                 (* k (ft ft (- k 1)))))))
        10) genv)

