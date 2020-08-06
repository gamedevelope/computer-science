#lang sicp

; (square x) 求x的平方
(define (square x) (* x x))

; 快速求幂
(define (fast-expt p n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt p (/ n 2))))
        (else (* p (fast-expt p (- n 1))))))

; (remainder a b) 求a除以b的余数

; gcd 求最大公约数
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 35 10)
(gcd 10 35)
(gcd 100 36)

; 1.2.6
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next-test-divisor n)
    (+ n 1))
  (define (next-test-divisor-v2 n)
    (if (> n 2) (+ n 2) (+ n 1)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-test-divisor-v2 test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; 一个属的幂对另外一个数取模的结果
 (define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; 用这种方法计算，遇到特别大的数时，会溢出
;(define (expmod base exp m)
;  (remainder (fast-expt base exp) m))

; 费马测试
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

; 检查某个数是否是素数
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; 练习 1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

; 练习 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

;(define (start-prime-test n start-time)
;  (if (prime? n)
;      (report-prime n (- (runtime) start-time))))

(define (start-prime-test n start-time)
  (if (fast-prime? n (- n 1))
      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n start-time)
  ; (display (string-append " search prime > " (number->string n)))
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      (search-for-primes (+ n 1) start-time)))

; 采用 fast-prime? 1596452
; (search-for-primes 1000000000000000 (runtime))

; 采用 prime? 1718863
(search-for-primes 1000000000000000 (runtime))

