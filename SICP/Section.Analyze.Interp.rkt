#lang sicp

(#%require "FigureCommon.scm")

;;; 惰性求值
(define (eval exp env)
  ((analyze exp) env))

;;; 惰性求值解释器
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((let? exp) (analyze-let exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

;;; 原生值
(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))

;;; TODO
(define (quoted? exp)
  false)

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

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

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (text-of-quotation exp) (cadr exp))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

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

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (if-predicate exp) (cadr exp))
  (define (if-consequent exp) (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))

(define (true? x) (not (eq? x false)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

;;; TODO 求值 let
(define (analyze-let exp)
  (lambda (env)
    (define (eval-let exp)
      (if (list? (cadr exp))
          ;;; 普通 let
          (let ((definitions (cadr exp))
                (body (cddr exp)))
            (let ((lbd (append (list 'lambda (map car definitions)) body)))
              (let ((lbdval ((analyze-lambda lbd) env)))
                (let ((mp (map (lambda (x) (eval (cadr x) env)) definitions)))
                  (execute-application lbdval mp)))))
          ;;; 命名 let
          (let ((funcname (cadr exp))
                (definitions (caddr exp))
                (body (cdddr exp)))
            (let ((func (append (list 'define (cons funcname (map car definitions))) body)))
              (let ((funcval (analyze-definition func)))
                (let ((e (cons funcname (map (lambda (x) (eval (cadr x))) definitions))))
                  (eval e)))))))
    (eval-let exp)))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (lambda-parameters exp) (cadr exp))
  (define (lambda-body exp) (cddr exp))
  (define (eval-lambda exp env)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

;(define (analyze-sequence exps)
;  (define (sequentially proc1 proc2)
;    (lambda (env) (proc1 env) (proc2 env)))
;
;  (define (loop first-proc rest-procs)
;    (if (null? rest-procs)
;        first-proc
;        (loop (sequentially first-proc (car rest-procs))
;              (cdr rest-procs))))
;  (let ((procs (map analyze exps)))
;    (if (null? procs)
;        (error "Empty sequence -- ANALYZE"))
;    (loop (car procs) (cdr procs))))

;;; 练习 4.23
(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else
           ((car procs) env)
           (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))

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
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

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
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;;; 
(define (setup-environment)
  (define primitive-procedures
    (list (list 'car car)
          (list 'cdr cdr)
          (list 'cons cons)
          (list 'null? null?)
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

(eval '(+ 1 1) genv)
;(eval '((lambda (x y) (* x y)) 10 11) genv)
(eval '(let ((a 1) (b 2)) (+ a b)) genv)
(eval '(let ((a 10))
         (let ((b 20))
           (* a b))) genv)
(eval '(let ((a (list 1 2 3 4 5)))
         (cdr a)) genv)
(eval '(let ((f (lambda (x) (* x x x))))
         (f 11)) genv)
(eval '(let ()
         (display 1)
         (display 2)) genv)

(eval '(define (fib n)
         (if (<= n 2)
             1
             (+ (fib (- n 1)) (fib (- n 2))))) genv)

;;; 练习 4.24
;(define t1 (runtime))
;(eval '(fib 25) genv)
;(define t2 (runtime))
;(display (list "时间差 " (- t2 t1)))

(eval '(if (< 1 2) 1 (/ 1 0)) genv)
(eval '(define (unless condition usual-value exceptional-value)
         (if condition exceptional-value usual-value)) genv)
;;; 练习 4.25
;;; 在正则序语言中会死循环
(eval '(define (factorial n)
         (unless (= n 1)
           (* n (factorial (- n 1)))
           1)) genv)
;(eval '(factorial 5) genv)

;;; 练习 4.22 特殊形式的 let
(eval '(let () (+ 1 2)) genv)
(eval '(let () (+ (let () (* 100 100)) 2)) genv)
(eval '(let () 1) genv)
