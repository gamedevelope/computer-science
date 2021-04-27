#lang sicp

(#%require "FigureCommon.scm")

;;; 惰性求值
(define (eval exp env)
  ((analyze exp) env))

;;; 
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((let? exp) (analyze-let exp))
        ((amb? exp) (analyze-amb exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

;;; 原生值
(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))

(define (quoted? exp)
  (and (pair? exp) (equal? 'quote (car exp))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (variable? exp)
  (symbol? exp))

(define (assignment? exp)
  (equal? 'set! (car exp)))

(define (if? exp)
  (equal? 'if (car exp)))

(define (let? exp)
  (equal? 'let (car exp)))

(define (lambda? exp)
  (equal? 'lambda (car exp)))

(define (begin? exp)
  (equal? 'begin (car exp)))

(define (begin-actions exp)
  (cdr exp))

(define (cond? exp)
  (equal? 'cond (car exp)))

(define (definition? exp)
  (equal? 'define (car exp)))

(define (application? exp)
  (pair? exp))

(define (text-of-quotation exp) (cadr exp))

(define (lookup-variable-value var env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((null? vars)
               (env-loop (enclosing-environment env)))
              ((eq? var (car vars)) (car vals))
              (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
          (error "Unbound variable" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))
    (env-loop env))
(define (set-variable-value! var val env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((null? vars)
               (env-loop (enclosing-environment env)))
              ((eq? var (car vars)) (set-car! vals val))
              (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
          (error "Unbound variable: SET!" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))
    (env-loop env))
(define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
(define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)
                     (cddr exp))))
(define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))


(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (enclosing-environment env) (cdr env))
(define the-empty-environment '())
(define (first-frame env) (car env))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (if-predicate exp) (cadr exp))
  (define (if-consequent exp) (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))

(define (true? x) (not (eq? x false)))

;;;SECTION 4.1.2
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))
(define (analyze-self-valuating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         (lambda (a-value fail2)
           (b env succeed fail2))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs)
       env
       (lambda (arg fail2)
         (get-args (cdr aprocs)
                   env
                   (lambda (args fail3)
                     (succeed (cons arg args) fail3))
                   fail2))
       fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")
(define (prompt-for-input string)
  (newline)(newline)(display string)(newline))
(define (announce-output string)
  (newline)(display string)(newline))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;; TODO 求值 let
;;; 需要定义出 let
;;; 练习 4.35需要
(define (analyze-let exp)
  (lambda (env succeed fail)
    (define (eval-let exp)
      (if (list? (cadr exp))
          ;;; 普通 let
          (let ((definitions (cadr exp))
                (body (cddr exp)))
            (let ((lbd (append (list 'lambda (map car definitions)) body)))
              (let ((v (analyze-lambda lbd)))
;                (display "============")
;                (newline)
;                (display v)
;                (newline)
;                (let ((lbdval ((analyze-lambda lbd) env succeed fail)))
                (let ((mp (map (lambda (x) (eval (cadr x) env)) definitions)))
                  (display mp)
                    ;;; TODO let 需要处理
                  (extend-environment v mp env)))))
          ;;; 命名 let
          (let ((funcname (cadr exp))
                (definitions (caddr exp))
                (body (cdddr exp)))
            (let ((func (append (list 'define (cons funcname (map car definitions))) body)))
              (let ((funcval (analyze-definition func)))
                (let ((e (cons funcname (map (lambda (x) (eval (cadr x))) definitions))))
                  (eval e)))))))
    (succeed (eval-let exp) fail)))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (eval-lambda exp env)
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))

(define (expand-clauses clauses)
    (define (make-if predicate consequent alternative)
      (list 'if predicate consequent alternative))
    
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause isn't last: COND->IF"
                         clauses))
              (make-if (cond-predicate first)
                       (sequence->exp (cond-actions first))
                       (expand-clauses rest))))))
(define (make-begin seq) (cons 'begin seq))
(define (cond-actions clause) (cdr clause))
(define (cond-predicate clause) (car clause))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (cond-clauses exp) (cdr exp))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (extend-environment vars vals base-env)
  (define (make-frame variables values)
    (cons variables values))
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define (procedure-parameters p) (cadr p))
(define (procedure-environment p) (cadddr p))
(define (procedure-body p) (caddr p))

;;; 
(define (setup-environment)
  (define primitive-procedures
    (list (list 'car car)
          (list 'cdr cdr)
          (list 'cons cons)
          (list 'null? null?)
          (list 'pair? pair?)
          ;          (list 'and and)
          ;          (list 'or or)
          (list 'not not)
          (list '+ +)
          (list '- -)
          (list '* *)
          (list '/ /)
          (list '= =)
          (list '< <)
          (list '<= <=)
          (list '> >)
          (list '>= >=)
          (list 'make-vector make-vector)
          (list 'vector-set! vector-set!)
          (list 'display display)
          (list 'list list)))
  
  ;;; 基本过程名
  (define (primitive-procedure-names)
    (map car primitive-procedures))
  
  ;;; 基本过程列表
  (define (primitive-procedure-objects)
    (map (lambda (proc) (list 'primitive (cadr proc)))
         primitive-procedures))
  
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define genv (setup-environment))

;;; 练习 4.35
(define (an-integer-between a b)
  (require (not (> a b)))
  (amb a (an-integer-between (+ a 1) b)))

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     genv
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     (lambda ()
                       (announce-output ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (aeo items)
  (require (pair? items))
  (amb (car items) (aeo (cdr items))))

(define f (let ((a 1)) a))

(define (two-sum s list1 list2)
  ((let ((a (aeo list1)))
    (let ((b (aeo list2)))
      (display (list a b))))))
;      (require (= s (+ a b)))
;      (list a b))))