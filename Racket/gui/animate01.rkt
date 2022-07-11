#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(define (child-scene elapse)
  (underlay/xy (rectangle 1000 1000 "solid" "black")
               500
               500
               ;(+ 100 (* 100 (sin (/ elapse 30.0))))
               ;(+ 100 (* 100 (sin (/ elapse 30.0))))
               (scale 2 (child elapse))))

(define x (scale 1 (bitmap "child.png")))

(define gv4 (vector
             (place-image x -32 -48 (empty-scene 32 48 (make-color 0 0 0 0)))
             (place-image x 0 -48 (empty-scene 32 48 (make-color 0 0 0 0)))
             (place-image x 32 -48 (empty-scene 32 48 (make-color 0 0 0 0)))
             (place-image x 64 -48 (empty-scene 32 48 (make-color 0 0 0 0)))))

(define child
  (lambda (elapse)
    (underlay/align "center"
                    "center"
                    (vector-ref gv4 (remainder (floor (/ elapse 5)) (vector-length gv4)))
                    (empty-scene 0 0))))

(animate child-scene)
