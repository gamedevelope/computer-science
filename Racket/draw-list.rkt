#lang racket

(require htdp/image)

(define c1 (rectangle 30 30 'outline (make-color 255 0 0)))
(define c2 (rectangle 30 30 'outline (make-color 0 0 0)))
(define dx 40)
(define dy 40)

(define s (empty-scene 1000 1000))
(define (f x y items s c)
  (cond [(null? items) s]
        [(pair? (car items))
         (begin
           (f x (+ dy y) (cdr items) (f x (+ dy y) (car items) s c) c))]
        [else
         (begin
           (f (+ dx x) y (cdr items) (place-image c x y s) c))]))
(define lst (list (cons 1 2) 3 4))
(f 100 100 lst s c1)
