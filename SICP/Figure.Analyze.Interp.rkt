#lang sicp
(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluation? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyzed-quoted exp))
        ((variable? exp) (analyzed-variable exp))
        ((assignment? exp) (analyzed-assignment exp))
        ((definition? exp) (analyzed-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))