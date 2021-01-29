#lang racket

(require db)
(define conn
  (mysql-connect #:server "192.168.43.128"
                 #:port 3306
                 #:database "douyu_bigdata"
                 #:user "root"
                 #:password "123456"))

(define rows (query-list conn
                         "select id from rooms order by id desc limit 10"))


(define (sum x)
  (if (null? x)
      1
      (* (car x) (sum (cdr x)))))