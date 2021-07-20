(define (test-and-set! cell)
  (if (car cell) true (begin (set-car! cell true) false)))
(define (clear! cell) (set-car! cell false))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (parallel-execute . thunks)
  (let ((my-threads '()))
    (define (terminator)
      (without-interrupts
       (lambda ()
         (for-each kill-thread my-threads)
         (set! my-threads '())
         unspecific)))
    (without-interrupts
     (lambda ()
       (set! my-threads
             (map (lambda (thunk)
                    (let ((thread (create-thread #f thunk)))
                      (detach-thread thread)
                      thread))
                  thunks))
       unspecific))
    terminator))

(define s (make-serializer))

(define x 10)
(define lst (list))

(parallel-execute
 (lambda () (set! x (* x x)))
 (lambda () (set! x (+ x 1))))

(display x)
(display lst)

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define a (make-account 100))
((a 'withdraw) 10)
((a 'withdraw) 10)
((a 'withdraw) 10)
(a 'balance)

; 练习 3.41
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) ((protected (lambda () balance))))
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define a (make-account 100))
(a 'balance)
((a 'withdraw) 10)
(a 'balance)

;(set! (a 'balance) 1000)
;(a 'balance)

; 练习 3.42
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) protected-withdraw)
              ((eq? m 'deposit) protected-deposit)
              ((eq? m 'balance) balance)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))
      dispatch)))

(define a (make-account 100))
((a 'withdraw) 10)
(a 'balance)

((a 'withdraw) 10)
(a 'balance)

(parallel-execute
 (lambda () ((a 'withdraw) 10))
 (lambda () ((a 'withdraw) 10))
 (lambda () ((a 'withdraw) 10)))

(a 'balance)

(let ()
  (display "hello")
  (sleep-current-thread 500)
  (display "world"))

; 使用多重共享资源的复杂性
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))


(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

(define a1 (make-account-and-serializer 100))
(define a2 (make-account-and-serializer 200))

(serialized-exchange a1 a2)

(a1 'balance)
(a2 'balance)

; 练习 3.44
; 练习 3.45
; 这样会死锁
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit aount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define a1.3.45 (make-account-and-serializer 1000))
(define a2.3.45 (make-account-and-serializer 500))

(serialized-exchange a1.3.45 a2.3.45)

; 练习 3.46
(define (make-mutex-n n)
  (display "make-mutex-n"))
