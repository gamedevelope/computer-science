#lang sicp

(#%require "Figure.4.4.4.rkt")

(aroa! '(rule (address (Bitdiddle Ben) (Slumerville (ridge Road) 10))))
(aroa! '(rule (job (Bitdiddle Ben) (computer wizard))))
(aroa! '(rule (salary (Bitdiddle Ben) 60000)))

(aroa! '(rule (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))))
(aroa! '(rule (job (Hacker Alyssa P) (computer programmer))))
(aroa! '(rule (salary (Hacker Alyssa P) 40000)))
(aroa! '(rule (supervisor (Hacker Alyssa P) (Bitdiddle Ben))))

(aroa! '(rule (address (Fect Cy D) (Cambridge (Ames Street) 3))))
(aroa! '(rule (job (Fect Cy D) (computer programmer))))
(aroa! '(rule (salary (Fect Cy D) 35000)))
(aroa! '(rule (supervisor (Fect Cy D) (Bitdiddle Ben))))

(aroa! '(rule (address (Tweakit Lem E) (Boston (Bay State Road) 22))))
(aroa! '(rule (job (Tweakit Lem E) (computer technician))))
(aroa! '(rule (salary (Tweakit Lem E) 25000)))
(aroa! '(rule (supervisor (Tweakit Lem E) (Bitdiddle Ben))))

(aroa! '(rule (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))))
(aroa! '(rule (job (Reasoner Louis) (computer programmer trainee))))
(aroa! '(rule (salary (Reasoner Louis) 30000)))
(aroa! '(rule (supervisor (Reasoner Louis) (Hacker Alyssa P))))

(aroa! '(rule (supervisor (Bitdiddle Ben) (Warbukers Oliver))))

(aroa! '(rule (address (Warbucks Oliver) (Swellesley (Top Heap Road)))))
(aroa! '(rule (job (Warbucks Oliver) (administration big wheel))))
(aroa! '(rule (salary (Warbucks Oliver) 150000)))

(aroa! '(rule (address (Scrooge Eben) (Weston (Shady Lane) 10))))
(aroa! '(rule (job (Scrooge Eben) (accounting chief accountant))))
(aroa! '(rule (salary (Scrooge Eben) 75000)))
(aroa! '(rule (supervisor (Scrooge Eben) (Warbucks Oliver))))

(aroa! '(rule (address (Cratchet Robert) (Allston (N Harvard Street) 16))))
(aroa! '(rule (job (Cratchet Robert) (accounting scriverner))))
(aroa! '(rule (salary (Cratchet Robert) 18000)))
(aroa! '(rule (supervisor (Cratchet Robert) (Scrooge Eben))))

(aroa! '(rule (address (Aull DeWitt) (Slumerville (Onion Square) 5))))
(aroa! '(rule (job (Aull DeWitt) (administration secretary))))
(aroa! '(rule (salary (Aull DeWitt) 25000)))
(aroa! '(rule (supervisor (Aull DeWitt) (Warbucks Oliver))))

(aroa! '(rule (can-do-job (computer wizard) (computer programmer))))
(aroa! '(rule (can-do-job (computer wizard) (computer technician))))
(aroa! '(rule (can-do-job (computer programmer) (computer programmer trainee))))
(aroa! '(rule (can-do-job (administration secretary)
                          (administration big wheel))))

(aroa! '(rule (meeting accounting (Monday 9am))))
(aroa! '(rule (meeting administration (Monday 10am))))
(aroa! '(rule (meeting computer (Wednesday 3pm))))
(aroa! '(rule (meeting administration (Friday 1pm))))
(aroa! '(rule (meeting whole-company (Wednesday 4pm))))

(aroa! '(rule (son Adam Cain)))
(aroa! '(rule (son Cain Enoch)))
(aroa! '(rule (son Enoch Irad)))
(aroa! '(rule (son Irad Mehujael)))
(aroa! '(rule (son Mehujael Methushael)))
(aroa! '(rule (son Methushael Lamech)))
(aroa! '(rule (wife Lamech Ada)))
(aroa! '(rule (son Ada Jabal)))
(aroa! '(rule (son Ada Jubal)))

;(aroa! '(rule (same ?x ?x)))
(query-driver-loop)

;(define assert! 0)
;;; 4.55
;(supervisor ?x (Bitdiddle Ben))
;(job ?x (accounting . ?y))
;(address ?x (Slumerville . ?y))

;;; 4.56
;(and (supervisor ?x (Bitdiddle Ben))
;     (address ?x . ?y))

;(and (salary ?person ?amount)
;     (salary (Bitdiddle Ben) ?max)
;     (lisp-value <= ?amount ?max))

;(and (job ?person ?y)
;     (supervisor ?person ?manager)
;     (not (job ?manager (computer . ?x))))

;;; 定义 same

'(assert! (rule (same ?x ?x)))

;;; 4.57
; 有问题的解答
; 存在另一种情况 a可以代替b, b可以代替c，那么a也可以代替c
'(assert! (rule (can-replace? ?p1 ?p2)
                (or (and (job ?p1 ?j1)
                         (job ?p2 ?j2)
                         (not (same ?p1 ?p2))
                         (or (can-do-job ?j1 ?j2)
                             (same ?j1 ?j2))))))

'(can-replace? ?x (Fect Cy D))
'(can-replace? ?x (Reasoner Louis))
'(can-replace? ?x (Tweakit Lem E))

;;; 4.58
'(assert! (rule (wheel ?person)
                (and (supervisor ?middle-manager ?person)
                     (supervisor ?x ?middle-manager))))

'(assert! (rule (big-wheel ?person)
                (and (supervisor ?x ?person)
                     (not (and (supervisor ?person ?y))
                          (job ?person (?j . ?rest-1))
                          (job ?y (?j . ?rest-2))))))

;;; 4.59
; (meeting ?x (Friday ?t))
'(assert! (rule (meeting-time ?person ?day-and-time)
                (or (and (job ?person (?a . ?b))
                         (meeting ?a ?day-and-time))
                    (meeting whole-company ?day-and-time))))

'(meeting-time (Hacker Alyssa P) (Wednesday ?t))

;;; 4.60
;;; 每种组合会出现两次，所以会出现重复的结果
;;; 解决这种问题，可以设置一个权重，只允许名字按权重从小到大出现
'(assert! (rule (lives-near ?p1 ?p2)
                (and (address ?p1 (?town . ?rest-1))
                     (address ?p2 (?town . ?rest-2))
                     (not (same ?p1 ?p2)))))
;                     (lisp-value string<?
;                                 (name->string ?p1)
;                                 (name->string ?p2)))))

;;; 4.61
'(assert! (rule (?x next-to ?y in (?x ?y . ?u))))
'(assert! (rule (?x next-to ?y in (?v . ?z))
                (?x next-to ?y in ?z)))
'(?x next-to ?y in (1 (2 3) 4))
'(?x next-to 1 in (2 1 3 1))

;;; 4.62
;;; 定义 last-pair
;;; (last-pair (list 1 2 3 4 5)) => (5)

;;; (last-pair (3) ?x)
;;; (last-pair (1 2 3) ?x)
;;; (last-pair (2 ?x) (3))
'(assert! (rule (last-pair (?x . ()) (?x))))
'(assert! (rule (last-pair (?x . ?z) ?last-elem)
                (last-pair ?z ?last-elem)))

;;; 4.63
'(assert! (rule (is-son ?m ?s)
                (or (and (son ?w ?s)
                         (wife ?m ?w))
                    (son ?m ?s))))

'(assert! (rule (grandson ?s ?g)
                (and (is-son ?s ?f)
                     (is-son ?f ?g))))

'(?x Adam Irad) ;; 搜索 Adam Irad 之间是什么关系

;;; 4.64
'(assert! (rule (outranked-by ?staff-person ?boss)
                (or (supervisor ?staff-person ?boss)
                    (and (outranked-by ?middle-manager ?boss)
                         (supervisor ?staff-person ?middle-manager)))))

;;; 4.65
;;; 出现多次是因为 Bitdiddle Ben 含有多个下属, 匹配到了多个管理链表

;;; 4.68
'(assert! (rule (append-to-form () ?y ?y)))
'(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
                (append-to-form ?v ?y ?z)))

; '(assert! (rule (reverse () ())))
'(assert! (rule (reverse (?x . ()) (?x))))
'(assert! (rule (reverse (?x . ?y) (?u . ?v))
                (and (reverse ?y ?t)
                     (append-to-form ?t (?x) (?u . ?v)))))

;;; 4.69
;;; 4.70
;;; 这样定义会将 THE-ASSERTION 设置成一个无穷流

;;; 4.71
'(define (simple-query query-pattern frame-stream)
   (stream-flatmap
    (lambda (frame)
      (stream-append (find-assertions query-pattern frame)
                     (apply-rules query-pattern frame)))
    frame-stream))

'(define (disjoin disjuncts frame-stream)
   (if (empty-disjunction? disjuncts)
       the-empty-stream
       (interleave
        (qeval (first-disjunct disjuncts) frame-stream)
        (disjoin (rest-disjuncts disjuncts) frame-stream))))

;;; 4.72
;;; 疑问：什么是交错方式? 具体的代码形式是什么？

;;; 4.73
'(define (flatten-stream stream)
   (if (stream-null? stream)
       the-empty-stream
       (interleave
        (stream-car stream)
        (flatten-stream (stream-cdr stream)))))
