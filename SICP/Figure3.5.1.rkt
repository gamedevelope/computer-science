(cons-stream 1 2)
(define cs (cons-stream 1 2))
(stream-null? cs)

(stream-car cs)
(stream-cdr cs)

(define (stream-values s n)
  (if (= n 0)
      '()
      (cons
       (stream-car s) 
       (stream-values (stream-cdr s) (- n 1)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream
          (stream-car stream)
          (stream-filter pred (stream-cdr stream))))
        (else
         (stream-filter pred (stream-cdr stream)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define s1 (stream-enumerate-interval 1 10))
(stream-car s1)
(stream-car (stream-cdr s1))
(stream-car (stream-cdr (stream-cdr s1)))

(display-stream s1)

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))
(stream-car integers)
(stream-car (stream-cdr integers))
(stream-car (stream-cdr (stream-cdr integers)))

(define (fib-starting-from m n)
  (cons-stream m (fib-starting-from n (+ m n))))

(define fib (fib-starting-from 0 1))
(define (fib-end n)
  (define (loop)
    (let ((p (stream-car fib)))
      (if (< p n)
          (begin (display p)
                 (display " ")
                 (set! fib (stream-cdr fib))
                 (loop)))))
  (loop))

(fib-end 1000)

(define s2 (stream-map (lambda (x) (+ x x)) s1))
(display-stream s2)

(define (foo x . y)
  (display (list x y)))
(foo 1 2 3 4 5 6)

; 练习 3.50
; 需要深刻理解 map 的作用
(define (stream-map-v2 proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map-v2
              (cons proc (map stream-cdr argstreams))))))

(define s3 (stream-map-v2 * s2 s2 s2))
(display s1)
(display s2)
(display-stream s3)

; 练习 3.51
(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)
(stream-ref x 10)

; 练习 3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(stream-ref y 7)
(display-stream z)

(define p 10)
(apply + (list 1 2 3 4))
(map + (list 1 2 3 4) (list 1 2 3 4) (list 1 2 3 4))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))
(stream-ref no-sevens 100)

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))
(stream-ref fibs 20)

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))
(stream-values primes 100)

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map-v2 + s1 s2))
(define integers
  (cons-stream 1 (add-streams ones integers)))
(stream-values integers 10)

(define fibs
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))
(stream-values fibs 10)

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))
(define double (cons-stream 1 (scale-stream double 2)))
(stream-values double 10)

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else
           (iter (stream-cdr ps)))))
  (iter primes))

(stream-values primes 10)

; 练习 3.53
(define s (cons-stream 1 (add-streams s s)))
(stream-values s 10)

; 练习 3.54
(define (mul-streams s1 s2) (stream-map-v2 * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams factorials (integers-starting-from 1))))
(stream-values factorials 10)

; 练习 3.55
(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))
(stream-values (partial-sums integers) 10)

; 练习 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car
                               (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car
                               (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))
(define s-3.56
  (cons-stream 1 (merge (scale-stream s-3.56 2)
                        (merge (scale-stream s-3.56 3)
                               (scale-stream s-3.56 5)))))
(stream-values (stream-cdr s-3.56) 10)

; 练习 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den)
           den
           radix)))
(stream-values (expand 1 7 10) 30)
(stream-values (expand 3 8 10) 30)
(stream-values (expand 1 3 10) 30)

; 练习 3.59

; 3.5.3 流计算模式的使用
(define (average x y)
  (/ (+ x y) 2))

; 计算 pi
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map-v2 - (pi-summands (+ n 2)))))
(stream-values (pi-summands 1) 10)

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(stream-values pi-stream 20)

; 加速流计算 pi
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(stream-values (euler-transform pi-stream) 20)

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(stream-values (accelerated-sequence euler-transform pi-stream) 10)

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(stream-values (sqrt-stream 2) 10)

; 练习 3.63
; 重复计算了 sqrt-stream x
; 如果采用 (lambda () <exp>)实现, 两个版本效率差别很大
(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))
(stream-values (sqrt-stream 2) 10)

; 练习 3.64
(define (stream-limit s tolerance)
  (let ((v1 (stream-car s))
        (v2 (stream-car (stream-cdr s))))
    (if (< (abs (- v1 v2)) tolerance)
        v2
        (stream-limit (stream-cdr s) tolerance))))

(define (sqrt-3.64 x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt-3.64 2 0.0001)

; 练习 3.65
; 计算 ln 2
; 0.69314718056
(define (e-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map-v2 - (e-summands (+ n 1)))))

(stream-values (e-summands 1) 10)
(stream-values (partial-sums (e-summands 1)) 10)
(define e0 (euler-transform (partial-sums (e-summands 1))))
(define e1 (euler-transform e0))
(define e2 (euler-transform e1))
(define e3 (euler-transform e2))
(stream-values (euler-transform e3) 10)
;(define pi-stream
;  (scale-stream (partial-sums (pi-summands 1)) 4))
;
;(stream-values pi-stream 20)

; 序对的无穷流
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map-v2 (lambda (x) (list (stream-car s) x))
                   (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(stream-values (pairs integers integers) 20)
(define plist (stream-values (pairs integers integers) 20))

(define (square-pairs p)
  (if (null? p)
      '()
      (let ((fp (car p)))
        (cons (+ (square (car fp)) (square (cadr fp)))
              (square-pairs (cdr p))))))
(square-pairs plist)

; 练习 3.66
; (1, 100) 之前有 197 个 (1,n)之前有2n-3 个 (n > 1)
; (100, 100) 之前有

; 将流作为信号
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

; 练习 3.67

(define (pairs-3.67 s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     ; 第一行
     (stream-map-v2 (lambda (x) (list (stream-car s) x))
                    (stream-cdr t))
     ; 第一列
     (stream-map-v2 (lambda (x) (list x (stream-car t)))
                    (stream-cdr s)))
    (pairs-3.67 (stream-cdr s) (stream-cdr t)))))

(stream-values (pairs-3.67 integers integers) 20)

; 练习 3.68
; 无穷递归
;(define (pairs s t)
;  (interleave
;   (stream-map (lambda (x) (list (stream-car s) x))
;               t)
;   (pairs (stream-cdr s) (stream-cdr t))))
;
;(stream-values (pairs integers integers) 20)

; 练习 3.69
(define (sum-square a b)
  (+ (square a) (square b)))

(define (check sp sq)
  (let ((p (stream-car sp))
        (k (stream-car sq)))
    (let ((i (car p))
          (j (cadr p)))
      (let ((v1 (sum-square i j))
            (v2 (square k))
            (v3 (sqrt (sum-square i j))))
        (cond ((integer? v3)
               (cons-stream (list i j v3)
                            (check (stream-cdr sp) sq)))
              ((< v2 v3)
               (check (stream-cdr sp) sq))
              (else
               (check sp (stream-cdr sq))))))))

(define (triples s t u)
  (let ((p (pairs s t)))
    (check p u)))

(stream-values (check (pairs integers integers) integers) 5)

; 练习 3.70
(define (weight a b)
  (+ a b))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1v1 (stream-car s1))
               (s2v1 (stream-car s2)))
           (let ((w1 (weight (car s1v1) (cadr s1v1)))
                 (w2 (weight (car s2v1) (cadr s2v1))))
             (cond ((<= w1 w2)
                    (cons-stream s1v1 (merge-weighted (stream-cdr s1) s2 weight)))
                   (else
                    (cons-stream s2v1 (merge-weighted s1 (stream-cdr s2) weight)))))))))

(define (pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map-v2 (lambda (x) (list (stream-car s) x))
                   (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define s (pairs integers integers weight))
(stream-values s 30)

; 练习 3.70
(define (weight i j)
  (+ (* 2 i)
     (* 3 j)
     (* 5 i j)))
(define s (pairs (stream-cdr s-3.56) (stream-cdr s-3.56) weight))
(stream-values s 10)

(define (sum-weight v)
  (let ((i (car v))
        (j (cadr v)))
    (weight i j)))

(define (calc-stream s)
  (cons-stream (sum-weight (stream-car s))
               (calc-stream (stream-cdr s))))

(stream-values (calc-stream s) 10)

; 练习 3.71
(define (sum-weight i j)
  (+ (* i i i)
     (* j j j)))

(define (cube v)
  (let ((i (car v))
        (j (cadr v)))
    (sum-weight i j)))

(define s0 (pairs integers integers sum-weight))
(define s1 (stream-map (lambda (x) (cube x)) s0))

(define (fs stream)
  (cond ((stream-null? stream) the-empty-stream)
        (else
         (let ((s (stream-cdr stream)))
           (let ((v1 (stream-car stream))
                 (v2 (stream-car s)))
             (if (= v1 v2)
                 (cons-stream v1 (fs s))
                 (fs s)))))))
(stream-values (fs s1) 10)

; 练习 3.72
(define (weight i j)
  (+ (* i i)
     (* j j)))

(define (f v)
  (let ((i (car v))
        (j (cadr v)))
    (+ (* i i) (* j j))))

(define s0 (pairs integers integers weight))

(define (fs s)
  (cond ((stream-null? s) the-empty-stream)
        (else
         (let ((s1 (stream-cdr s)))
           (let ((s2 (stream-cdr s1)))
             (let ((v0 (stream-car s))
                   (v1 (stream-car s1))
                   (v2 (stream-car s2)))
               (cond ((and (= (f v0) (f v1))
                           (= (f v0) (f v2)))
                      (cons-stream (list v0 v1 v2)
                                   (fs (stream-cdr s2))))
                     (else
                      (fs (stream-cdr s2))))))))))
(define s1 (fs s0))
(stream-car s1)
(stream-car (stream-cdr s1))
(stream-values s1 5)

; 将流作为信号
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

; 练习 3.73
(define s (integral (scale-stream ones 1) 100 1))
(stream-values s 10)

(define (rc R C dt)
  (define (int is v)
    (add-streams (integral is v (* (/ 1 C) dt))
                 (scale-stream is R)))
  int)

(define rc1 (rc 5 1 0.5))
(stream-values (rc1 ones 100) 10)

; 练习 3.74
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define nums
  (cons-stream 1
               (stream-map (lambda (x) (* -1 x)) nums)))
(stream-values nums 10)
(define (sign-change-detector i j)
  (cond ((and (> i 0) (< j 0)) 1)
        ((and (< i 0) (> j 0)) -1)
        (else 0)))

(define zero-crossings (make-zero-crossings nums 0))
(stream-values zero-crossings 10)

(define zero-crossings
  (stream-map-v2 sign-change-detector
                 nums
                 (cons-stream (stream-car nums)
                              nums)))

(stream-values zero-crossings 10)

; 练习 3.75
(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-value)
                 (make-zero-crossings (stream-cdr input-stream)
                                      avpt))))
; 改正后
(define (make-zero-crossings input-stream prev-avpt last-value)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt prev-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      avpt
                                      (stream-car input-stream)))))
(stream-values nums 10)
(define zero-crossings
  (make-zero-crossings nums 0 0))
(stream-values zero-crossings 10)

; 练习 3.76
(define (smooth s)
  (if (stream-null? s)
      the-empty-stream
      (let ((s1 (stream-cdr s)))
        (if (stream-null? s1)
            the-empty-stream
            (let ((v1 (stream-car s))
                  (v2 (stream-car (stream-cdr s))))
              (cons-stream (/ (+ v1 v2) 2)
                           (smooth (stream-cdr s))))))))

(stream-values (smooth integers) 10)
(define s (cons-stream 1
                       (cons-stream 2
                                    (cons-stream 3 the-empty-stream))))
(stream-values (smooth s) 2)
(exit)
