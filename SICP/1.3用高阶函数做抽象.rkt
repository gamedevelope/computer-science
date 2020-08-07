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