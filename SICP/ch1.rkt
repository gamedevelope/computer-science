#lang racket

(require "common.rkt")
(require racket/trace)
(require rackunit)

;;; Exercise 1.3
(define (ex1.3)
  (define (f a b c)
    (if (and (<= a b) (<= a c))
        (+ (square b) (square c))
        (f b c a)))

  (println (f 1 2 3))
  (println (f 4 3 2)))

;;; Exercise 1.8
(define (ex1.8)
  (define (cube-root x)
    (define (cube-root-iter guess)
      (if (good-enough? guess)
          guess
          (cube-root-iter (improve guess))))
  
    (define (good-enough? guess)
      (< (abs (- (cube guess) x)) 0.001))
  
    (define (improve y)
      (/ (+ (/ x (square y))
            (* 2 y))
         3.0))
    (cube-root-iter 1))

  (println (cube-root 3))
  (println (cube (cube-root 3)))

  (println (cube-root -10))
  (println (cube (cube-root -10))))

;;; Exercise 1.10
(define (ex1.10)
  (define (A x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1) (A x (- y 1))))))
  (println (A 1 10)) ; 1024
  (println (A 2 4))  ; 65536
  (println (A 3 3))  ; 65536

  (define (f n) (A 0 n)) ; (define (f n) (* 2 n))
  (println (f 11))
  
  (define (g n) (A 1 n)) ; (define (g n) (expt 2 n))
  (println (g 11))

  (define (h n) (A 2 n))
  ;  (define (h n)
  ;    (if (<= n 1)
  ;        2
  ;        (expt 2 (h (- n 1)))))
  ; (h n) => 2 ^ 2 ^ 2 ... ^ 2 count 2 is n
  (println (h 4))
  )
(ex1.10)

;;; Exercise 1.11
;;; 递归版本
(define (ex1.11-v1)
  (define (f n)
    (if (< n 3)
        n
        (+ (f (- n 1))
           (* 2 (f (- n 2)))
           (* 3 (f (- n 3))))))
  (lambda-cost (lambda ()(f 15))))
(ex1.11-v1)
;;; 迭代版本
(define (ex1.11-v2)
  (define (f n)
    (define (iter x v1 v2 v3)
      (if (<= n x)
          v1
          (iter (+ x 1)
                (+ v1 (* 2 v2) (* 3 v3))
                v1
                v2)))
    (if (< n 3)
        n
        (iter 2 2 1 0)))
  (println (f 1))
  (println (f 2))
  (println (f 3))
  (lambda-cost (lambda ()(f 15)))
  )
(ex1.11-v2)

;;; Exercise 1.12
;;; 帕斯卡三角形
;;; 由于本章还没有开始介绍 cons, car, cdr 等操作
;;; 所以暂时只提供 (f x y) 来表示第几行第几个数
(define (ex1.12)
  (define (f x y)
    (cond ((< x y) (error "x must <= y"))
          ((= y 1) 1)
          ((= x y) 1)
          (else
           (+ (f (- x 1) (- y 1))
              (f (- x 1) y)))))

  (println (list (f 4 1) (f 4 2) (f 4 3) (f 4 4)))
  (println (list (f 5 1) (f 5 2) (f 5 3) (f 5 4) (f 5 5))))
(ex1.12)

;;; Exercise 1.13
;;; 根据提示很容易证明结论

;;; Exercise 1.14

;;; Exercise 1.16
(define (ex1.16)
  ;;; 使用迭代的方式求幂
  ;;; n不能是负整数
  (define (iter p x n)
    (cond ((= n 0) 1)
          ((= n 1) p)
          ((odd? n) (iter (* p x) x (- n 1)))
          (else
           (iter (* p p) x (/ n 2)))))
  (define (fast-expt x n)
    (iter x x n))
  (println (fast-expt 1 4))
  (println (fast-expt 2 0))
  (println (fast-expt 2 4))
  (println (fast-expt 2 100))
  )
(ex1.16)

;;; Exercise 1.17
(define (ex1.17)
  (define (fast-mul a b)
    (cond ((= a 0) 0)
          ((= b 0) 0)
          ((= b 1) a)
          ((= b -1) (- a))
          ((< a b) (fast-mul b a))
          ((even? b) (fast-mul (double a) (halve b)))
          (else
           (+ a (fast-mul a (- b 1))))))
  (check-equal? (fast-mul 10 10) (* 10 10))
  (println (fast-mul 10 10))
  (println (fast-mul 99 99))
  (println (fast-mul 2 -1024))
  )
(ex1.17)

;;; Exercise 1.18 跟 1.17 要求一样
;;; 换成迭代的计算方式
(define (ex1.18)
  (define (iter r a b)
    (cond [(= b 0) r]
          [(even? b)
           (iter r (double a) (halve b))]
          [else
           (iter (+ r a)
                 a
                 (- b 1))]))
  (define (fast-mul a b)
    (iter 0 a b))
  (check-equal? (fast-mul 10 10) (* 10 10))
  (check-equal? (fast-mul 0 0) (* 0 0))
  (check-equal? (fast-mul 1 1) (* 1 1))
  (println (fast-mul 1024 1023))
  )
(ex1.18)

;;; Exercise 1.19
;(define (ex1.19)
;  (define (fib n)
;    (fib-iter 1 0 0 1 n))
;  (define (fib-iter a b p q count)
;    (cond [(= count 0) b]
;          [(even? count)
;           (fib-iter a
;                     b
;                     ..
;                     ..
;                     (/ count 2))]
;          [else
;           (fib-iter (+ (* b q) (* a q) (* a p))
;                     (+ (* b p) (* a q))
;                     p
;                     q
;                     (- count 1))]))
;  (println (fib 10))
;  )
;(ex1.19)

; 1.2.6

(define (prime? n)
  (= n (smallest-divisor n)))

(define (ex1.21)
  (println (smallest-divisor 199))
  (println (smallest-divisor 1999))
  (println (smallest-divisor 19999))
  )
(ex1.21)

(define (ex1.22)
  (define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))

  (define (start-prime-test n start-time)
    (if (fast-prime? n 100)
        (report-prime n (- (runtime) start-time))
        #f))
  
  (define (report-prime n elapsed-time)
    (display n)
    (display " **** ")
    (display elapsed-time)
    (newline)
    #t)

  (define (search-for-primes low count)
    (if (= count 0)
        #f
        (if (start-prime-test low (runtime))
            (search-for-primes (inc low) (dec count))
            (search-for-primes (inc low) count))))
  
  (search-for-primes 1000 3)
  (newline)
  (search-for-primes 100000000 3)
  (newline)  
  (search-for-primes 1000000000 3)
  
  ;  (timed-prime-test 11258998397111)
  ;  (timed-prime-test 1125899839733759)
  )
(ex1.22)

;;; 
(define (ex1.23)
  (println "1.23")
  )
(ex1.23)
;;; ex1.24 因为CPU太快，在书上指定范围内已经测量不出性能差别
;;;

;;; ex1.25
;;; Alysss P.Hacker 说的不对
;;; 求特别大的数，会有明显差异
(define (ex1.25)
  (define (iter p x n)
    (cond ((= n 0) 1)
          ((= n 1) p)
          ((odd? n) (iter (* p x) x (- n 1)))
          (else
           (iter (* p p) x (/ n 2)))))
  (define (fast-expt x n)
    (iter x x n))
  (define (expmod-v1 base exp m)
    (remainder (fast-expt base exp) m))

  (define (expmod-v2 base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder
          (square (expmod-v2 base (/ exp 2) m))
          m)]
        [else
         (remainder
          (* base (expmod-v2 base (- exp 1) m))
          m)]))

  (println "ex1.25")

  (trace iter)
  (println (expmod-v1 10 10 3))

  (trace expmod-v2)
  (println (expmod-v2 105 11 11))
  )
(ex1.25)

;;; 多次重复计算 expmod 导致性能下降
(define (ex1.26)
  ;;; (base ^ exp) mod m
  (define (expmod base exp m)
    (cond [(= exp 0) 1]
          [(even? exp)
           (remainder
            (* (expmod base (/ exp 2) m)
               (expmod base (/ exp 2) m))
            m)]
          [else
           (remainder
            (* base (expmod base (- exp 1) m))
            m)]))
  (trace expmod)
  (expmod 10 100 3)
  )
;(ex1.26)

(define (ex1.27)
  (define (try-it a n)
    (cond [(>= a n) #t]
          [(= (expmod a n n) a) (try-it (add1 a) n)]
          [else #f]))
  (define (fermat-test n)
    (display "fermat-test ")
    (display n)
    (display " result: ")
    (try-it 1 n))
  
  (println (fermat-test 3))
  (println (fermat-test 4))
  (println (fermat-test 5))
  (println (fermat-test 6))

  ;;; 能通过费马检查的数
  ;;; 561, 1105, 1729, 2465, 2821, and 6601.
  (println (fermat-test 561))
  (println (fermat-test 1105))
  (println (fermat-test 1729))
  (println (fermat-test 2465))
  (println (fermat-test 2821))
  (println (fermat-test 6601))
  )
(ex1.27)

(define (ex1.28)
  (define (expmod-v1.28 base exp m)
    ;;; 在这个方法中检测
    (define (square-test p)
      (if (and (> p 1)
               (< p (- m 1))
               (= 1 (remainder (square p) m)))
          0
          (square p)))
    (cond [(= exp 0) 1]
          [(even? base)
           (remainder (double (remainder (expmod-v1.28 (halve base) exp m)
                                         m))
                      m)]
          [(even? exp)
           (remainder
            (square-test (expmod-v1.28 base (/ exp 2) m))
            m)]
          [else
           (remainder
            (* base (expmod-v1.28 base (- exp 1) m))
            m)]))
  
  (define (fermat-test n)
    (define (try-it-v1.28 a)
      (= (expmod-v1.28 a n n) a))
    (try-it-v1.28 (+ 1 (random (- n 1)))))
  
  (define (miller-rabin-test? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (miller-rabin-test? n (- times 1)))
          (else false)))
  
  ;;; 检测次数足够大才能排错
  ;;; 这些都不是素数
  (println (miller-rabin-test? 561 10))
  (println (miller-rabin-test? 1105 10))
  (println (miller-rabin-test? 1729 10))
  (println (miller-rabin-test? 2465 10))
  (println (miller-rabin-test? 2821 10))
  (println (miller-rabin-test? 6601 10))
  
  ;;; 是素数
  (println (miller-rabin-test? 1009 10))
  )
(ex1.28)