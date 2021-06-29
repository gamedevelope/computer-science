#lang racket
(string->keyword "apple")
(define dir (find-system-path 'temp-dir))
(with-output-to-file (build-path dir "stuff.txt")
  (lambda () (printf "example\n")))