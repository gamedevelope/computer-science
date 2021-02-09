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

(eval '(define x 100) genv)
(eval '(set! x 2000) genv)