#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define ones (cons-stream 1 ones))
(define (stream-map op s1 s2)
  (cons-stream (op (stream-car s1)
                   (stream-car s2))
               (stream-map op (stream-cdr s1) (stream-cdr s2))))
(define n (stream-map + ones ones))
(define integer (cons-stream 1 (stream-map + ones integer)))
(define (stream-view n stream)
  (if (= n 0)
      '()
      (cons (stream-car stream)
            (stream-view (- n 1)
                         (stream-cdr stream)))))