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
                 (line 20 0 (make-color 255 0 0))
                 20
                 -5
                 (rotate 30 (regular-polygon 10 3 "solid" "red"))))

(define arrow-u (rotate 90 arrow-r))
(define arrow-l (rotate 180 arrow-r))
(define arrow-d (rotate 270 arrow-r))

(place-image arrow-l 20 20 s)
(place-image arrow-u 40 40 s)
(place-image arrow-r 60 60 s)
(place-image arrow-d 80 80 s)

(image-height (text "10" 18 "black"))
(image-width (text "AI" 18 "black"))

(overlay/xy
 (rectangle 30 30 "outline" "blue")
 10
 6
 (text "10" 18 "black"))
;(rotate 90 arrow-r)