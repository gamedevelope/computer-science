#lang racket

(require plot)
;(plot (function tan (- pi) pi #:label "y = tan(x)"))
;(plot (function (lambda (x) (* x x)) (- 10) 10 #:label "y = x^2"))
;(plot (function (lambda (x) (* x x x)) (- 10) 10 #:label "y = x^3"))
;(plot (function (lambda (x) (* x x x x)) (- 10) 10 #:label "y = x^4"))
;(plot (function abs (- 10) 10 #:label "y = abs(x)"))
;(plot (function (lambda (x) (/ 1 x)) (- 10) 10))
;(plot (function (lambda (x) (+ (* x x) (* 2 x) 1)) (- 10) 10))
; (plot3d (surface3d (λ (x y) (* (cos x) (sin y)))
;                    (- pi) pi (- pi) pi)
;         #:title "An R × R → R function"
;         #:x-label "x" #:y-label "y" #:z-label "cos(x) sin(y)")
;(parameterize ([plot-title  "An R × R → R function"]
;                 [plot-x-label "x"]
;                 [plot-y-label "y"]
;                 [plot-z-label "cos(x) sin(y)"])
;    (plot3d (contour-intervals3d (λ (x y) (* (cos x) (sin y)))
;                                 (- pi) pi (- pi) pi)))

(define (f x)
  (if (and (> x (- pi)) (< x pi))
      (cos x)
      (abs (/ 1 x))))
(plot (function f (- 1) 1))