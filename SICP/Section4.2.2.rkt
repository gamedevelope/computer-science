#lang sicp
(#%require "FigureCommon.scm")

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (actual-value exp env)
  (force-it (eval exp env)))

;(define (force-it obj)
;  (if (thunk? obj)
;      (actual-value (thunk-exp obj) (thunk-env obj))
;      obj))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

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

(define (assignment? exp) (tagged-list? exp 'set!))
(define (definition? exp) (tagged-list? exp 'define))
(define (if? exp) (tagged-list? exp 'if))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
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
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
(define (cond-actions clause) (cdr clause))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (application? exp)
  (pair? exp))
;  (tagged-list? exp 'call))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (enclosing-environment env) (cdr env))
(define the-empty-environment '())
(define (first-frame env) (car env))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (procedure-paramters p) (cadr p))

(define (apply procedure arguments env)
  (cond
    ; 基本过程
    ((primitive-procedure? procedure)
     (apply-primitive-procedure
      procedure
      (list-of-arg-values
       arguments env)))
    ; 复合过程
    ((compound-procedure? procedure)
     (eval-sequence
      (procedure-body procedure)
      (extend-environment
       (procedure-parameters procedure)
       (list-of-delayed-args
        arguments env)
       (procedure-environment procedure))))
    (else
     (error
      "Unknown procedure type -- APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define (procedure-parameters p) (cadr p))
(define (procedure-environment p) (cadddr p))
(define (procedure-body p) (caddr p))

(define (primitive-implementation proc) (cadr proc))
(define (make-frame variables values)
  (cons variables values))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))
;(define (eval-sequence exps env)
;  (cond ((last-exp? exps) (eval (first-exp exps) env))
;        (else (eval (first-exp exps) env)
;              (eval-sequence (rest-exps exps) env))))

;;; 练习 4.30
;;; 对 sequence 中每一项都进行实际求值
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

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


(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

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

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))


(define (self-evaluationg? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))


(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define primitive-procedures
  (list (list '= =)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'display display)
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        )
  )
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))
  
(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
;(define (driver-loop)
;  (prompt-for-input input-prompt)
;  (let ((input (read)))
;    (let ((output
;           (actual-value input genv)))
;      (announce-output output-prompt)
;      (user-print output)))
;  (driver-loop))
;(define (user-print object)
;  (if (compound-procedure? object)
;      (display (list 'compound-procedure
;                     (procedure-parameters object)
;                     (procedure-body object)
;                     '<procedure-env>))
;      (display object)))

;(define (announce-output string)
;  (newline)
;  (display string)
;  (newline))
;
;(define (prompt-for-input string)
;  (newline)
;  (newline)
;  (display string)
;  (newline))

(define genv (setup-environment))
(eval '(cons 1 2) genv)
(eval '(+ 1 2) genv)

;;; 练习 4.27
(eval '(define count 0) genv)
(eval '(define (id x)
         (set! count (+ count 1))
         x) genv)

;(driver-loop)

(eval '(define (try a b)
         (if (= a 0) 1 b)) genv)
(eval '(try 0 (/ 1 0)) genv)

;;; 练习 4.28
(eval '(define (square x) (* x x)) genv)
(eval '(square (id 10)) genv)
;;; 没有记忆功能时 count 为 2
;;; 有记忆功能时 count 为 1
(eval 'count genv)

(eval '(define (for-each proc items)
         (if (null? items)
             'done
             (begin (proc (car items))
                    (for-each proc (cdr items))))) genv)
;(eval '(for-each (lambda (x) (display x)) (list 1 2 3 4 5)) genv)
;;; 练习 4.30
(eval '(define (p1 x)
         (set! x (cons x '(2)))
         x) genv)
(eval '(define (p2 x)
         (define (p e)
           e
           x)
         (p (set! x (cons x '(2))))) genv)
(eval '(begin
        (p1 1)
        (p2 1)) genv)

;;; 4.2.3 将流作为惰性的表
(eval '(define (cons x y)
         (lambda (m) (m x y))) genv)
(eval '(define (car z)
         (z (lambda (p q) p))) genv)
(eval '(define (cdr z)
         (z (lambda (p q) q))) genv)
(eval '(define x (cons 1 2)) genv)
(eval '(car x) genv)
(eval '(cdr x) genv)

(eval '(define (list-ref items n)
         (if (= n 0)
             (car items)
             (list-ref (cdr items) (- n 1)))) genv)

(eval '(define (map proc items)
         (if (null? items)
             '()
             (cons (proc (car items))
                   (map proc (cdr items))))) genv)

(eval '(define (scale-list items factor)
         (map (lambda (x) (* x factor))
              items)) genv)

(eval '(define (add-lists list1 list2)
         (cond ((null? list1) list2)
               ((null? list2) list1)
               (else (cons (+ (car list1) (car list2))
                           (add-lists (cdr list1) (cdr list2)))))) genv)

(eval '(define ones (cons 1 ones)) genv)
(eval '(define integers (cons 1
                              (add-lists ones integers))) genv)
(force-it (eval '(list-ref ones 1) genv))
(force-it (eval '(list-ref integers 10) genv))

;;; 练习 4.32
(eval '(define (integral integrand initial-value dt)
         (define int
           (cons initial-value
                 (add-lists (scale-list integrand dt)
                            int)))
         int) genv)
(eval '(define (solve f y0 dt)
         (define y (integral dy y0 dt))
         (define dy (map f y))
         y) genv)
(force-it (eval '(list-ref (solve (lambda (x) x) 1 0.001) 1000) genv))

;;; 练习 4.33
(eval '1 genv)
(eval '(car '(a b c)) genv)


;;; 练习 4.34
