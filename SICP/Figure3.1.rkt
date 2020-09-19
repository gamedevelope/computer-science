#lang sicp
(#%require "Common.rkt")
; 3.1 累加器
(define (make-accumulator n)
  (lambda (x)
    (begin (set! n (+ n x))
           n)))

(define A (make-accumulator 0))
(define B (make-accumulator 10))

; 3.2 函数计数器
(define (make-monitored f)
  (let ((count 0))
    (lambda (input)
      (cond ((eq? input 'how-many-calls?) count)
            ((eq? input 'reset-count) (begin (set! count 0) true))
            (else
             (if (number? input)
                 (begin (set! count (inc count))
                        (f input))
                 (error "PARAM MUST BENUBER --" input)))))))
(let ((s (make-monitored sqrt)))
  (echo (s 100))
  (echo (s 10000))
  (echo (s 'how-many-calls?))
  (echo (s 'reset-count))
  (echo (s 100))
  (echo (s 10000))
  (echo (s 'how-many-calls?))
  (echo (s 'reset-count)))

; 3.3 带密码的账户
; 3.4 连续错误7次，报警
(define (make-account balance password)
  (define max-count 7)
  (define count 7)

  ; 校验密码
  (define (check-password pw)
    (if (eq? password pw)
        true
        (begin (set! count (dec count))
               (if (<= count 0)
                   (error "Call police")
                   (error "Incorrect password")))))

  ; 提款
  (define (withdraw amount pw)
    (if (check-password pw)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   (set! count max-count)
                   balance)
            (error "Insufficient funds"))))

  ; 取款
  (define (deposit amount pw)
    (if (check-password pw)
        (begin
          (set! balance (+ balance amount))
          (set! count max-count)
          balance)))

  ; dispatch
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'vc) count)
          (else
           (error "Unknow request -- MAKE-ACCOUNT"
                  m))))
  dispatch)

(let ((acc (make-account 100 '123)))
  (echo ((acc 'withdraw) 10 '123))
  (echo ((acc 'withdraw) 10 '123))
  (echo ((acc 'withdraw) 10 '123))
  (echo ((acc 'deposit) 1000 '123)))

(define rand
  (let ((x 1970))
    (lambda ()
      (begin (set! x (rand-update x))
             x))))

; 蒙特卡罗计算 pi
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (let ((n1 (rand))
        (n2 (rand)))
    (display (list "rand: " n1 n2))
    (= (gcd (rand) (rand)) 1)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (begin
             (display (list trials-passed trials))
             (/ trials-passed trials)))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(let ()
  (estimate-pi 100))