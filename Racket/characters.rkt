#lang racket

(integer->char 65)
(char->integer #\A)
(char-alphabetic? #\A)
(char-numeric? #\0)
(char-ci=? #\a #\A)
(char-ci=? #\a #\B)