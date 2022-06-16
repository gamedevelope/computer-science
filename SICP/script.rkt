#lang racket

(require "common.rkt")
(require racket/trace)

;;; Miller–Rabin Test
(expmod 1 2 31)

(define (f n)
  (define (iter x)
    (cond [(>= x n) '()]
          [(= 1 (expmod x 2 n)) (cons x (iter (inc x)))]
          [else (iter (inc x))]))
  (iter 1))

(define (g n)
  (define (iter x)
    (if (>= x n)
        '()
        (cons (expmod x 2 n)
              (cons (expmod (square (expmod x 1 n)) 1 n)
                    (iter (inc x))))))
  (iter 1))