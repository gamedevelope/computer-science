#lang sicp

(#%require "Figure4.3.3.rkt")


(define (analyze input)
  (define (succeed v next)
    'ok)

  (define (fail v next)
    false)
  
  (ambeval input genv succeed fail))

(analyze '(define (parse-sentence)
            (list 'sentence
                  (parse-noun-phrase)
                  (parse-word verbs))))
(analyze '(define (parse-noun-phrase)
            (list 'noun-phrase
                  (parse-word articles)
                  (parse-word nouns))))