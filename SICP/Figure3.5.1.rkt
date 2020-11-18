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