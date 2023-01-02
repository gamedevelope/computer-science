#lang racket

(require redis)
;(parse-redis-url "redis://127.0.0.1")
(define out (open-output-file "source.rkt"
                              #:mode 'text               
                              #:exists 'can-update))
(define c (make-redis #:client-name "aaa"
                      #:host "192.168.43.128"))
(redis? c)

(redis-set-add! c "a" "a")
(redis-set-count c "a")

(define (incr-loop key n)
  (let ((v (redis-bytes-incr! c key 0)))
    (if (< v n)
        (begin (redis-bytes-incr! c "num" 1)
               (incr-loop key n))
        v)))
(incr-loop "num" 100)
(redis-bytes-incr! c "num" 0)

(string->number (bytes->string/utf-8 (redis-bytes-get c "num")))
