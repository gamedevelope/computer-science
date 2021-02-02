#lang sicp

(#%provide eval
           genv)

(#%require "FigureCommon.scm")

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define the-empty-environment '())
(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))
; 练习 4.11 修改 env 的数据结构
; 练习 4.12 将 define-variable! set-variable-value! lookup-variable-value
; 合成一个结构
(define (define-variable! var val env)
  (define (add-binding-to-frame! var val frame)
    (set-cdr! frame
              (cons (cons var val)
                    (cdr frame))))
  
  (let ((frame (first-frame env)))
    (define (scan vals)
      (cond ((null? vals)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vals)) (set-cdr! (car vals) val))
            (else (scan (cdr vals)))))
    (scan frame)))

(define (env-loop var val f env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop var val f (enclosing-environment env)))
            ((eq? var (caar frame)) (f frame))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))

(define (set-variable-value! var val env)
  (define (f frame)
    (set-cdr! (car frame) val))
  (env-loop var val f env))

(define (lookup-variable-value var env)
  (env-loop var false cdar env))

;; 求值方法
(define (eval exp env)
  (define (self-evaluating? exp)
    (or (number? exp) (string? exp)))  
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

;; begin
(define (install-begin)
  (define (begin-actions exp) (cdr exp))
  (define (eval-begin exp env)
    (eval-sequence (begin-actions exp) env))
  (put 'eval 'begin eval-begin))
(install-begin)

;; call 函数调用
(define (install-call)
  (define (call exp env)
;    (display env)
    (apply (eval (operator (operands exp)) env)
           (list-of-values (operands (operands exp)) env)))
  (put 'eval 'call call))
(install-call)

;;; cond 语法
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
    (if (null? variables)
        '((() ()))
        (cons (cons (car variables) (car values))
              (make-frame (cdr variables) (cdr values)))))
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

;;; 定义 do
;;; do 的语法
;;;(do ((vec (make-vector 5))
;;;     (i 0 (+ i 1)))
;;;    ((= i 5) vec)
;;;  (vector-set! vec i i))    =>  #(0 1 2 3 4)
;;;
;;;(let ((x '(1 3 5 7 9)))
;;;  (do ((x x (cdr x))
;;;       (sum 0 (+ sum (car x))))
;;;      ((null? x) sum)))     =>  25

;;;(let ((x '(1 3 5 7 9)))
;;;  (do ((x x (cdr x))
;;;       (sum 1 (+ sum (car x)))
;;;       (s 1 (* s sum)))
;;;    ((null? x) s)))
;;; 转换后
;;;(let ((x '(1 3 5 7 9)))
;;;  (define (next x sum s)
;;;    (if (null? x)
;;;        s
;;;        (next (cdr x) (+ sum (car x)) (* s sum))))
;;;  (next x 1 1))

;;;
;;;(do ((x 0 (+ x 1))
;;;     (f (lambda (x) x)
;;;        (lambda (x) (f (+ x x)))))
;;;  ((> x 9) (f x)))
;;; 转换后
;;;(let ((x 0)
;;;      (f (lambda (x) x)))
;;;  (define (next x f)
;;;    (if (> x 9)
;;;        (f x)
;;;        (next (+ x 1) (lambda (x) (f (+ x x))))))
;;;  (next x f))

;;;
;;;(do ((vec (make-vector 5))
;;;     (i 0 (+ i 1)))
;;;  ((= i 5) vec)
;;;  (vector-set! vec i i))
;;; 转换后
;;;(let ()
;;;  (define (next vec i)
;;;    (if (= i 5)
;;;        vec
;;;        (begin (vector-set! vec i i)
;;;               (next vec (+ i 1)))))
;;;  (next (make-vector 5) 0))

;;;(do ((x 100)
;;;     (y 2 (+ y 1)))
;;;  ((> x y) x)
;;;  y)
;;; 转换后
;;;(let ((x 100)
;;;      (y 2))
;;;  (define (__iter y)
;;;    (if (> x y)
;;;        x
;;;        (begin
;;;          y
;;;          (__iter (+ y 1)))))
;;;  (__iter y))
(define (install-do)
  (define (do->let exp)
  (define (parse-params exp initialization iterator)
    (if (null? exp)
        (cons initialization iterator)
        (let ((first (car exp))
              (rest (cdr exp)))
          (if (= (length first) 2)
              (parse-params rest (append initialization (list first)) iterator)
              (parse-params rest
                     (append initialization (list (list (car first) (cadr first))))
                     (append iterator (list (list (car first) (caddr first)))))))))
  (define (parse-cause exp else)
    (cons 'if (append (caddr exp) (list else))))
  
  (define (make-func exp iter)
    (cons 'begin (append (cdddr exp) (list iter))))
  
  (let ((params (parse-params (cadr exp) '() '())))
    (let ((initialization (car params))
          (iterator (cdr params)))
      (list 'let
            initialization
            (list 'define
                  (cons '__iter (map car iterator))
                  (parse-cause exp (make-func exp (cons '__iter (map cadr iterator)))))
            (cons '__iter (map car iterator))))))
  
  (define (eval-do exp env)
    (eval (do->let exp) env))
  
  (put 'eval 'do eval-do))
(install-do)

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
  
  (define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))
  
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
