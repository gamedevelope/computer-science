#lang sicp

(define (square x) (* x x))

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
((lambda (x) (+ 1 x)) 1)

; 使用 let
(define (test-let)
  (let ((a 'hello)
        (b 'world))
  (display a)
  (display b)))

(test-let)

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(f 10 20)
(+ (let ((x 3))
     (+ x (* x 10)))
   3)
(define (ff g)
  (g 2))
(ff square)
(ff (lambda (x) (* x (inc x))))
; (ff ff)

; 章节 1.3.3
; 使用 half-interval method 寻找方程的根
(define (average a b)
  (/ (+ a b) 2))

(define (close-enough? a b)
  (< (abs (- a b)) 0.00000001))

(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        mid-point
        (let ((test-value (f mid-point)))
          (cond ((positive? test-value)
                 (search f neg-point mid-point))
                ((negative? test-value)
                 (search f mid-point pos-point))
                (else mid-point))))))

(search (lambda (x) (* x x)) -1 1)
(search (lambda (x) (* x x x)) -10 1)

; 增加容错处理
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)
(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)

; 寻找函数的不动点
(display "寻找函数的不动点")
(newline)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)
(fixed-point (lambda (x) (+ (sin x) (cos x)))
             1.0)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(sqrt 100)

; 练习 1.35
; f(x) = 1 + 1/x 的不动点是黄金分割点
(newline)
(display "寻找 f(x) = 1 + 1/x 的不动点, 不使用 average damping 方法")
(fixed-point (lambda (x) (+ 1 (/ 1 x)))
             1.0)

(newline)
(display "寻找 f(x) = 1 + 1/x 的不动点, 使用 average damping 方法")
(fixed-point (lambda (x) (average x (+ 1 (/ 1 x))))
             1.0)

; 练习 1.36
; 打印寻找不动点过程中的一系列选值
(newline)
(display "求解 x^x = 1000 不使用 average damping 方法")
(fixed-point (lambda (x) (/ (log 1000) (log x)))
             2.0)

(newline)
(display "求解 x^x = 1000 使用 average damping 方法")
(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
             2.0)

; 练习 1.37 计算连分式的值
(newline)
(display "求解连分式的值")
(newline)
(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (d i)
        (/ (n i)
           (+ (d i)
              (iter (inc i))))))
  (iter 1))

; 迭代版本，不太容易想
(define (cont-frac-iter n d k)
  (define (iter i result)
    (cond ((= i 0) result)
          (else (let ((result (/ (n i)
                                 (+ (d i) result))))
                  (iter (dec i) result)))))
  (iter k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           20)

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                20)

(newline)
(display "练习1.38 用欧拉给出的连分式计算 e")
(newline)

; 练习 1.38
; 计算欧拉给出的计算 e 值的连分式
(define (double x) (+ x x))
(define (d i)
  (if (= (remainder i 3) 2)
      (double (/ (+ i 1) 3))
      1))


(+ 2 (cont-frac (lambda (i) 1.0)
                d
                500))

(+ 2 (cont-frac-iter (lambda (i) 1.0)
                     d
                     500))

; 练习 1.39
(define (f139 x n k)
  (if (= n k)
      (- (double n) 1)
      (- (double n)
         (g139 x n k))))

(define (g139 x n k)
  (/ (square x)
     (f139 x (inc n) k)))

(define (tan-cf x k)
  (/ x (f139 x 1 k)))

(tan-cf 3.1415926 10)
(tan-cf 1.0 10)
(tan 1.0)