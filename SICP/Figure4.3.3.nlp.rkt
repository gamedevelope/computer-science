#lang sicp

(#%require "Figure4.3.3.rkt")

(define (analyze input)
  (define (succeed v next)
    'ok)

  (define (fail v next)
    false)
  
  (ambeval input genv succeed fail))

(analyze '(define nouns '(noun student professor cat class 教授 苹果 学生 猫 教师)))
(analyze '(define verbs '(verb studies lectures eats sleeps 学习 睡觉 吃)))         
(analyze '(define articles '(article the a)))
;(analyze '(define 名词 '(名词 教授 学生 猫 教师)))
;(analyze '(define 动词 '(动词 学习 睡觉 吃)))

(analyze '(define (require p)
            (if (not p) (amb))))

(analyze '(define (parse-sentence)
            (list 'sentence
                  (parse-noun-phrase)
                  (parse-verb-phrase))))

(analyze '(define (parse-noun-phrase)
            (list 'noun-phrase
                  (parse-word articles)
                  (parse-word nouns))))

(analyze '(define (memq item x)
            (cond ((null? x) false)
                  ((eq? item (car x)) x)
                  (else (memq item (cdr x))))))

(analyze '(define (parse-word word-list)
            (require (not (null? *unparsed*)))
            (require (memq (car *unparsed*) (cdr word-list)))
            (let ((found-word (car *unparsed*)))
              (set! *unparsed* (cdr *unparsed*))
              (list (car word-list) found-word))))

(analyze '(define *unparsed* '()))
(analyze '(define (parse input)
            (set! *unparsed* input)
            (let ((sent (parse-sentence)))
              (require (null? *unparsed*))
              sent)))
;(analyze '(parse '(the cat eats)))
(analyze '(define prepositions '(prep for to in by with)))
(analyze '(define (parse-prepositional-phrase)
            (list 'prep-phrase
                  (parse-word prepositions)
                  (parse-noun-phrase))))
(analyze '(define (parse-verb-phrase)
            (define (maybe-extend verb-phrase)
              (amb verb-phrase
                   (maybe-extend (list 'verb-phrase
                                       verb-phrase
                                       (parse-prepositional-phrase)))))
            (maybe-extend (parse-word verbs))))
(analyze '(define (parse-simple-noun-phrase)
            (list 'simple-noun-phrase
                  (parse-word articles)
                  (parse-word nouns))))
(analyze '(define (parse-noun-phrase)
            (define (maybe-extend noun-phrase)
              (amb noun-phrase
                   (maybe-extend (list 'noun-phrase
                                       noun-phrase
                                       (parse-prepositional-phrase)))))
            (maybe-extend (parse-simple-noun-phrase))))
; (parse '(the student with the cat sleeps in the class))
(driver-loop)
