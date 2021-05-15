#lang sicp

(#%require "Figure4.3.3.rkt")


(define (analyze input)
  (define (succeed v next)
    'ok)

  (define (fail v next)
    false)
  
  (ambeval input genv succeed fail))

(analyze '(define nouns '(noun student professor cat class)))
(analyze '(define verbs '(verb studies lectures eats sleeps)))         
(analyze '(define articles '(article the a)))

(analyze '(define (require p)
            (if (not p) (amb))))

(analyze '(define (parse-sentence)
            (list 'sentence
                  (parse-noun-phrase)
                  (parse-word verbs))))
(analyze '(define (parse-noun-phrase)
            (list 'noun-phrase
                  (parse-word articles)
                  (parse-word nouns))))

(analyze '(define (parse-word word-list)
         (require (not (null? *unparsed*)))
         (require (memq (car *unparsed*) (cdr word-list)))
         (let ((found-word (car *unparsed*)))
           (set! *unparsed* (cdr *unparsed*))
           (list (car word-list) found-word))))

(analyze '(define *unparsed* '()))
(analyze '(define (parse input)
            (set! *unparsed* input)
            (let ((send (parse-sentence)))
              (require (null? *unparsed*))
              sent)))

(driver-loop)