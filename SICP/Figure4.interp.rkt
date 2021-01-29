#lang sicp

(#%provide eval
           genv)

(#%require "FigureCommon.scm")

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; 求值方法
(define (eval exp env)
  (define (self-evaluating? exp)
    (or (number? exp) (string? exp)))

  ;;; 查找基本过程、变量等
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
  
  (cond
    ;;; 数字、字符串 -- 直接返回本身
    ((self-evaluating? exp) exp)
    ;;; 变量 -- 从环境中查找对应的值
    ((symbol? exp) (lookup-variable-value exp env))
    ;;; 过程 -- 求这个过程的值
    (else
     (let ((proc (get 'eval (car exp))))
       (if proc
           (proc exp env)
           (let ((p (eval (operator exp) env))
                 (vals (list-of-values (operands exp) env)))
             (apply p vals)))))))
;                    (list-of-values (operands exp) env))))))))
;           (error "Unbound procedure " (car exp)))))))

;; begin
(define (install-begin)
  (define (begin-actions exp) (cdr exp))
  (define (eval-begin exp env)
    (eval-sequence (begin-actions exp) env))
  (put 'eval 'begin eval-begin))
(install-begin)

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; call 函数调用
(define (install-call)
  (define (call exp env)
;    (display env)
    (apply (eval (operator (operands exp)) env)
           (list-of-values (operands (operands exp)) env)))
  (put 'eval 'call call))
(install-call)

(define (install-cond)
  (define (make-begin seq) (cons 'begin seq))
  (define (cond-actions clause) (cdr clause))
  (define (cond-predicate clause) (car clause))
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
  (define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))
  
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
  (define (cond-clauses exp) (cdr exp))
  (define (cond->if exp) (expand-clauses (cond-clauses exp)))
  (define (eval-cond exp env)
    (eval (cond->if exp) env))
  (put 'eval 'cond eval-cond))
(install-cond)

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (enclosing-environment env) (cdr env))
(define the-empty-environment '())
(define (first-frame env) (car env))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (apply procedure arguments)
  (if (pair? procedure)
      (let ((proc (get 'apply (car procedure))))
        (if proc
            (proc procedure arguments)
            (error "Unknown procedure type -- APPLY" procedure)))))

(define (install-apply-primitive-procedure)
  (define (primitive-implementation proc) (cadr proc))
  (define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme
     (primitive-implementation proc) args))
  (put 'apply 'primitive apply-primitive-procedure))
(install-apply-primitive-procedure)

(define (install-apply-compound-procedure)
  (define (apply-compound-procedure procedure arguments)
    (eval-sequence
     (procedure-body procedure)
     (extend-environment
      (procedure-parameters procedure)
      arguments
      (procedure-environment procedure))))
  (put 'apply 'procedure apply-compound-procedure))
(install-apply-compound-procedure)

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

;;; lambda
(define (install-lambda)
  (define (make-procedure parameters body env)
    (list 'procedure parameters body env))
  (define (lambda-parameters exp) (cadr exp))
  (define (lambda-body exp) (cddr exp))
  (define (eval-lambda exp env)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env))
  (put 'eval 'lambda eval-lambda))
(install-lambda)

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (install-assignment)
  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env)
    'ok)

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

  (define (assignment-variable exp) (cadr exp))
  (define (assignment-value exp) (caddr exp))
  (put 'eval 'set! eval-assignment))
(install-assignment)

(define (install-if)
  (define (true? x) (not (eq? x false)))
  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))
  (define (if-predicate exp) (cadr exp))
  (define (if-consequent exp) (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))
  (put 'eval 'if eval-if))
(install-if)

(define (install-and)
  (define (eval-and exps env)
    (define (proc exps env)
      (cond ((last-exp? exps) (eval (first-exp exps) env))
            (else (let ((v (eval (first-exp exps) env)))
                    (if (eq? v false)
                        false
                        (proc (rest-exps exps) env))))))
    (proc (cdr exps) env))
  (put 'eval 'and eval-and))
(install-and)

(define (install-or)
  (define (eval-or exps env)
    (define (proc exps env)
      (cond ((last-exp? exps) (eval (first-exp exps) env))
            (else (let ((v (eval (first-exp exps) env)))
                    (if (eq? v true)
                        v
                        (proc (rest-exps exps) env))))))
    (proc (cdr exps) env))
  (put 'eval 'or eval-or))
(install-or)

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (list-of-values exps env)
  (define (no-operands? ops) (null? ops))
  (define (first-operand ops) (car ops))
  (define (rest-operands ops) (cdr ops))
  
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (install-quote)
  (define (text-of-quotation exp env) (cadr exp))
  (put 'eval 'quote text-of-quotation))
(install-quote)

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

(define (install-definition)
  (define (eval-definition exp env)
    (define-variable! (definition-variable exp)
      (eval (definition-value exp) env)
      env)
    'ok)
  
  (define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
  
  (define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))
  
  (define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)
                     (cddr exp))))
  (put 'eval 'define eval-definition))
(install-definition)

;;; 定义 let 语法糖
(define (install-let)
  (define (eval-let exp env)
    (if (list? (cadr exp))
        ;;; 普通 let
        (let ((definitions (cadr exp))
              (body (cddr exp)))
          (let ((lbd (append (list 'lambda (map car definitions)) body)))
            (let ((lbdval ((get 'eval 'lambda) lbd env)))
              (apply ((get 'eval 'lambda) lbd env)
                     (map (lambda (x) (eval (cadr x) env)) definitions)))))
        ;;; 命名 let
        (let ((funcname (cadr exp))
              (definitions (caddr exp))
              (body (cdddr exp)))
          (let ((func (append (list 'define (cons funcname (map car definitions))) body)))
            (let ((funcval ((get 'eval 'define) func env)))
              (let ((e (cons funcname (map (lambda (x) (eval (cadr x) env)) definitions))))
                (eval e env)))))))
  (put 'eval 'let eval-let))
(install-let)

;;; 定义 let*
;;; 写法有些繁琐
;;; TODO 简化
(define (install-let*->nested-lets)
  (define (let*->nested-lets exp)
    (cond ((not (pair? exp)) exp)
          ((eq? 'let* (car exp))
           (if (<= (length (cadr exp)) 1)
               (append (list 'let (cadr exp)) (let*->nested-lets (cddr exp)))
               (append (list 'let (list (let*->nested-lets (caadr exp))))
                       (list (let*->nested-lets (append (list 'let* (cdadr exp)) (let*->nested-lets (cddr exp))))))))
          (else
           (cons (let*->nested-lets (car exp))
                 (let*->nested-lets (cdr exp))))))
  (define (eval-let* exp env)
    (eval (let*->nested-lets exp) env))
  (put 'eval 'let* eval-let*))
(install-let*->nested-lets)

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

(define (driver-loop)
  (define input-prompt ";;; M-Eval input:")
  (define output-prompt ";;; M-Eval value:")
  
  (define (prompt-for-input string)
    (newline)
    (newline)
    (display string)
    (newline))
  
  (define (announce-output string)
    (newline)
    (display string)
    (newline))

  (define (compound-procedure? p)
    (tagged-list? p 'procedure))
  
  (define (user-print object)
    (if (compound-procedure? object)
        (display (list 'compound-procedure
                       (procedure-parameters object)
                       (procedure-body object)
                       '<procedure-env>))
        (display object)))
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input genv)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))