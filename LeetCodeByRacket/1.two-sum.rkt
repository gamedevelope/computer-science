#lang racket

(define/contract (two-sum nums target)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (define table (make-hash))
  (define (iter nums target index)
    (let ((n (car nums)))
      (let ((k (- target n)))  
        (if (hash-has-key? table k)
            (list (hash-ref table k) index)
            (begin (hash-set! table n index)
                   (iter (cdr nums) target (+ index 1)))))))
  (iter nums target 0))

(two-sum '(1 2 3 4) 3)
(two-sum '(1 2 3 4) 5)
(two-sum '(1 2 3 4) 6)
