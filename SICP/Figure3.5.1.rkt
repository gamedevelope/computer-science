(cons-stream 1 2)
(define cs (cons-stream 1 2))
(stream-null? cs)

(stream-car cs)
(stream-cdr cs)

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