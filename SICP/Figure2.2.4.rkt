#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(define wave einstein)
(paint einstein)
(define wave2
  (beside einstein (flip-vert einstein)))
(paint wave2)
(define wave4
  (below wave2 wave2))
(paint wave4)
(define wave8
  (below wave4 wave4))
(paint wave8)
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(define wave4-v2 (flipped-pairs einstein))
(paint wave4-v2)
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
(paint (right-split wave 3))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))