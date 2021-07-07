#lang racket

(define ht (make-hash))
(hash-set! ht "apple" '(red round))
(hash-set! ht "banana" '(yellow long))
(hash-ref ht "apple")
(car (hash-ref ht "apple"))
;(hash-ref ht "coconut")
(hash-ref ht "coconut" "not there")

(define ht2 #hash(("apple" . red)
                  ("nanana" . yellow)))
(hash-ref ht2 "apple")
(hash-count ht2)

(define ht3 (make-hash))
(hash-set! ht3 "China" '(abc aaa))

;;; ht4 是不可变hash
(define ht4 (hash "apple" 'red "banana" 'yellow))
(hash-ref ht4 "apple")
