#lang sicp

(#%require sicp-pict)

(define wave einstein)
; 输出画板
(paint wave)
; 水平翻转
(paint (flip-vert wave))
; 垂直翻转
(paint (flip-horiz wave))
; 旁边
(paint (beside wave wave))
; 上下
(paint (below wave wave))
(paint (beside (below wave wave) wave))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
(paint (right-split wave 3))

; 练习 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
(paint (up-split wave 3))

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
(paint (corner-split wave 3))

; 练习 2.45
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs-2.45 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(paint (flipped-pairs-2.45 wave))
(paint ((square-of-four identity flip-horiz flip-vert rotate180) wave))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(paint (square-limit wave 3))
(paint (right-split wave 2))

(define (split tl tr)
  (lambda (painter)
    (tl painter (tr painter painter))))

(define right-split-v2 (split beside below))
(paint (right-split-v2 wave))
(define up-split-v2 (split below beside))
(paint (up-split-v2 wave))

; 练习 2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect p)
  (car p))

(define (ycor-vect p)
  (cdr p))

(define (add-rect p1 p2)
  (cons (+ (xcor-vect p1) (xcor-vect p2))
        (+ (ycor-vect p1) (ycor-vect p2))))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

;(define (edge2-frame frame)
;  (caddr frame))

(define (edge2-frame frame)
  (cddr frame))

(define (scale-vect s v)
            (make-vect (* s (xcor-vect v))
                       (* s (ycor-vect v))))
                        
;(define (make-frame v1 v2 v3)
;  (list v1 v2 v3))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (frame-coord-map frame)
  (lambda (v)
    (add-rect
     (origin-frame frame)
     (add-rect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

((frame-coord-map (make-frame (make-vect 1 1) (make-vect -1 1) (make-vect 1 1))) (make-vect 0 0))
((frame-coord-map (make-frame (make-vect 1 1) (make-vect -1 1) (make-vect 1 1))) (make-vect 0 1))
((frame-coord-map (make-frame (make-vect 1 1) (make-vect -1 1) (make-vect 1 1))) (make-vect 1 0))
((frame-coord-map (make-frame (make-vect 1 1) (make-vect -1 1) (make-vect 1 1))) (make-vect 1 1))