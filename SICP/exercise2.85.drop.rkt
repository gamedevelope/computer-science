#lang sicp
(#%provide install-drop-package)

(define (install-drop-package)
  (define type-tower (list (cons (cons 'complex 'real)
                                 (lambda (x)
                                   (real-part x)))
                           (cons (cons 'real 'rational)
                                 (lambda (x)
                                   (make-rational (round x) 1)))
                           (cons (cons 'rational 'integer)
                                 (lambda (x)
                                   (numer x)))))
  "drop done")
