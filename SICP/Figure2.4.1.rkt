#lang sicp

; 复数的表示

;(make-from-real-imag (real-part z) (imag-part z))
;(make-from-mag-ang (magnitude z) (angle z))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (square x)
  (* x x))
; 直角坐标表示法
;(define (real-part z) (car z))
;(define (imag-part z) (cdr z))
;
;(define (magnitude z)
;  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
;
;(define (angle z)
;  (atan (imag-part z) (real-part z)))
;
;(define (make-from-real-imag x y) (cons x y))
;
;(define (make-from-mag-ang r a)
;  (cons (* r (cos a)) (* r (sin a))))

; 极坐标表示法
(define (real-part z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang r a)
  (cons r a))

(define z1 (make-from-real-imag 10 5))
(define z2 (make-from-real-imag 5 10))

(add-complex z1 z2)
(sub-complex z1 z2)
(mul-complex z1 z2)
(div-complex z1 z2)