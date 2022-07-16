#lang racket

;(require htdp/image)
(require 2htdp/image)


(define c1 (rectangle 30 30 'outline (make-color 255 0 0)))
(define c2 (rectangle 30 30 'outline (make-color 0 0 0)))
;(define dx 40)
;(define dy 40)
;
(define s (empty-scene 500 500))
;(define (f x y items s c)
;  (cond [(null? items) s]
;        [(pair? (car items))
;         (begin
;           (f x (+ dy y) (cdr items) (f x (+ dy y) (car items) s c) c))]
;        [else
;         (begin
;           (f (+ dx x) y (cdr items) (place-image c x y s) c))]))
;(define lst (list (list 1 2) 3 4))
;(f 100 100 lst s c1)

(define arrow-r (overlay/xy
 (text "   >" 12 (make-color 255 0 0))
 0
 10
 (line 20 0 (make-color 255 0 0))))

(place-image arrow-r 100 100 s)

;(rotate 90 arrow-r)