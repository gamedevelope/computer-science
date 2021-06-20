#lang sicp

(#%provide aroa!
           query-driver-loop)

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query result:")
(define (pfi string)
  (newline)
  (display string)
  (newline))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define THE-ASSERTIONS the-empty-stream)
(define THE-RULES the-empty-stream)
(define rule-counter 0)

(define (show-rule-count? exp)
  (eq? (type exp) 'show-rule-count))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (rule? statement)
  (tagged-list? statement 'rule))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

(define (use-index? pat)
  (constant-symbol? (car pat)))



(define (query-driver-loop)
  (pfi input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base."))
          ((show-rule-count? q)
           (display rule-counter))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                 frame
                 (lambda (v f)
                   (contract-question-mark v))))
             (qeval q (singleton-stream '()))))))
    (query-driver-loop)))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule)) '(always-true) (caddr rule)))

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))
(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))
(put 'or 'qeval disjoin)

(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))
(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
              call
            frame
            (lambda (v f)
              (error "Unknown pat var -- LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))
(put 'lisp-value 'qeval lisp-value)

(define (name->string lst frame-stream)
  (define (f lst)
    (if (null? lst)
        ""
        (string-append (car lst) (f (cdr lst)))))
  (f lst))
(put 'name->string 'qeval name->string)

;(define user-initial-environment (make-top-level-environment))
(define user-initial-environment (scheme-report-environment 5))

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (always-true ignore frame-stream) frame-stream)

(put 'always-true 'qeval always-true)

(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (cdr pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)
(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))
(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (define (store-rule-in-index rule)
    (let ((pattern (conclusion rule)))
      (if (indexable? pattern)
          (let ((key (index-key-of pattern)))
            (let ((current-rule-stream
                   (get-stream key 'rule-stream)))
              (put key
                   'rule-stream
                   (cons-stream rule
                                current-rule-stream)))))))
  
  (define (add-rule! rule)
    (store-rule-in-index rule)
    (let ((old-rules THE-RULES))
      (set! THE-RULES (cons-stream rule old-rules))
      'ok))

  (define (store-assertion-in-index assertion)
    (if (indexable? assertion)
        (let ((key (index-key-of assertion)))
          (let ((current-assertion-stream
                 (get-stream key 'assertion-stream)))
            (put key
                 'assertion-stream
                 (cons-stream assertion
                              current-assertion-stream))))))
  
  (define (add-assertion! assertion)
    (store-assertion-in-index assertion)
    (let ((old-assertions THE-ASSERTIONS))
      (set! THE-ASSERTIONS
            (cons-stream assertion old-assertions))
      'ok))
  
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))
(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))
(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

(define (add-assertion-body exp)
  (car (contents exp)))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (display-line x)
  (newline) (display x))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
                  (if (number? (cadr variable))
                      (string-append (symbol->string (caddr variable))
                                     "-"
                                     (number->string (cadr variable)))
                      (symbol->string (cadr variable))))))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable frame)
  (assoc variable frame))
(define (extend variable value frame)
  (cons (make-binding variable value) frame))

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknownexpression CONTENTS" exp)))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed
        (stream-cdr s1)
        delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed
        (force delayed-s2)
        (delay (stream-cdr s1))))))

(define aroa! add-rule-or-assertion!)

;
;(aroa! '(rule (address (Bitdiddle Ben) (Slumerville (ridge Road) 10))))
;(aroa! '(rule (job (Bitdiddle Ben) (computer wizard))))
;(aroa! '(rule (salary (Bitdiddle Ben) 60000)))
;(aroa! '(rule (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))))
;(aroa! '(rule (job (Hacker Alyssa P) (computer programmer))))
;(aroa! '(rule (salary (Hacker Alyssa P) 40000)))
;(aroa! '(rule (supervisor (hacker Alyssa P) (Bitdiddle Ben))))
;
;(aroa! '(rule (address (Fect Cy D) (Cambridge (Ames Street) 3))))
;(aroa! '(rule (job (Fect Cy D) (computer programmer))))
;(aroa! '(rule (salary (Fect Cy D) 35000)))
;(aroa! '(rule (supervisor (Fect Cy D) (Bitdiddle Ben))))
;
;(aroa! '(rule (address (Tweakit Lem E) (Boston (Bay State Road) 22))))
;(aroa! '(rule (job (Tweakit Lem E) (computer technician))))
;(aroa! '(rule (salary (Tweakit Lem E) 25000)))
;(aroa! '(rule (supervisor (Tweakit Lem E) (Bitdiddle Ben))))
;
;(aroa! '(rule (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))))
;(aroa! '(rule (job (Reasoner Louis) (computer programmer trainee))))
;(aroa! '(rule (salary (Reasoner Louis) 30000)))
;(aroa! '(rule (supervisor (Reasoner Louis) (Hacker Alyssa P))))
;
;(aroa! '(rule (supervisor (Bitdiddle Ben) (Warbukers Oliver))))
;
;(aroa! '(rule (address (Warbucks Oliver) (Swellesley (Top Heap Road)))))
;(aroa! '(rule (job (Warbucks Oliver) (administration big wheel))))
;(aroa! '(rule (salary (Warbucks Oliver) 150000)))
;
;(aroa! '(rule (address (Scrooge Eben) (Weston (Shady Lane) 10))))
;(aroa! '(rule (job (Scrooge Eben) (accounting chief accountant))))
;(aroa! '(rule (salary (Scrooge Eben) 75000)))
;(aroa! '(rule (supervisor (Scrooge Eben) (Warbucks Oliver))))
;
;(aroa! '(rule (address (Cratchet Robert) (Allston (N Harvard Street) 16))))
;(aroa! '(rule (job (Cratchet Robert) (accounting scriverner))))
;(aroa! '(rule (salary (Cratchet Robert) 18000)))
;(aroa! '(rule (supervisor (Cratchet Robert) (Scrooge Eben))))
;
;(aroa! '(rule (address (Aull Dewitt) (Slumerville (Onion Square) 5))))
;(aroa! '(rule (job (Aull DeWitt) (administration secretary))))
;(aroa! '(rule (salary (Aull Dewitt) 25000)))
;(aroa! '(rule (supervisor (Aull Dewitt) (Warbucks Oliver))))
;
;(aroa! '(rule (can-do-job (computer wizard) (computer programmer))))
;(aroa! '(rule (can-do-job (computer wizard) (computer technician))))
;(aroa! '(rule (can-do-job (computer programmer) (computer programmer trainee))))
;(aroa! '(rule (can-do-job (administration secretary)
;                          (administration big wheel))))
;
;(query-driver-loop)
