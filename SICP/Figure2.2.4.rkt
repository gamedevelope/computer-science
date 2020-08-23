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