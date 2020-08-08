#lang sicp

; 1.3 用高阶函数做抽象
(define (cube x) (* x x x))
(cube 1)
(cube 2)
(cube 3)

;; 1.3.1 过程作为参数
;; 求 a 到 b 的各整数之和
;(define (sum-integers a b)
;  (if (> a b)
;      0
;      (+ a (sum-integers (inc a) b))))
;(sum-integers 1 10)
;
;; 求a到b的立方和
;(define (sum-cubes a b)
;  (if (> a b)
;      0
;      (+ (cube a) (sum-cubes (inc a) b))))
;(sum-cubes 1 10)
;
;; 求a到b的序列之和
;(define (pi-sum a b)
;  (if (> a b)
;      0
;      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))
;(pi-sum 1 100)

; 抽象的求和方法
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(sum + 1 inc 10)

; 新定义的立方和
(define (sum-cubes a b)
  (sum cube a inc b))

; 恒等式
(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))
(sum-integers 1 10)

(define (pi-sum a b)
  (define (term a)
    (+ (/ 1.0 (* a (+ a 2)))))
  (define (next a)
    (+ a 4))
  (sum term a next b))
(pi-sum 1 10)

; 计算 pi 的近似值
(* 8 (pi-sum 1 10000))

; 计算一个定积分
;(define (integral f a b dx)
;  (define (add-dx x) (+ x dx))
;  (* (sum f (+ a (/ dx 2.0)) add-dx b)
;     dx))
;
;(integral cube 0 1 0.01) ; 0.24998750000000042

; 辛普森规则
(display "辛普森规则")
(newline)
(define (integral f a b n)
  ; 定义 h h = (b - a)/n
  (define h (/ (- b a) n))

  ; 定义每一项的求值方法
  (define (g k)
    ; 计算系数
    (define (fk k)
      (cond ((= k 0) 1)
            ((= k n) 1)
            (else
             (if (even? k) 2 4))))
    (* (fk k) (f (+ a (* k h)))))
  (* (/ h 3)
     (sum g 0 inc n)))
(integral cube 0 1 2)

; 练习 1.30, 写一个迭代过程的 sum
(display "练习 1.30")
(newline)
(define (sum-v2 term a next b)
  (define (iter a result)
    (if (> a b)
        0
        (+ (term a)
           result
           (iter (inc a) result))))
  (iter a 0))
(sum-v2 identity 1 inc 10)
(sum-v2 cube 1 inc 10)

; 练习 1.31
; 定义 product 计算乘积

(display "练习 1.31")
(newline)

; 递归计算过程
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))
(product identity 1 inc 10)

; 迭代计算过程
(define (product-v2 term a next b)
  (define (iter a result)
    (if (> a b)
        1
        (* (term a)
           result
           (iter (next a) result))))
  (iter a 1))
(product-v2 identity 1 inc 10)

; 利用乘积计算 pi 值
(define (factorial-pi n)
  (define (next p)
    (+ 2 p))
  (define (square p) (* p p))
  (define (term p)
    (/ (* p (+ 2 p)) (square (inc p))))

  (product term 2 next n))

; 计算 pi 值
(* 4.0 (factorial-pi 100))

(display "练习 1.32")
(newline)

; 定义 accumulate
; 递归计算过程
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

; 迭代版本
(define (accumulate-v2 combiner null-value term a next b)
  (define (iter combiner a result)
    (if (> a b)
        null-value
        (combiner (term a)
                  result
                  (iter combiner (next a) result))))
  (iter combiner a null-value))
(display "递归计算过程")
(newline)
(accumulate + 0 identity 1 inc 10)
(accumulate * 1 identity 1 inc 10)

(display "迭代计算过程")
(newline)
(accumulate-v2 + 0 identity 1 inc 10)
(accumulate-v2 * 1 identity 1 inc 10)

(display "练习 1.33")
(newline)
; 练习 1.33
; 定义 accumulate
; 递归计算过程
(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (combiner (if (filter a) (term a) null-value)
                (filtered-accumulate combiner null-value term (next a) next b filter))))

; gcd 求最大公约数
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (relative-prime a b)
  (= 1 (gcd a b)))

; 带过滤功能的累计
(filtered-accumulate * 1 identity 1 inc 10 (lambda (x) (= 1 (gcd x 10))))

; 章节 1.3.2
; 使用 lambda
