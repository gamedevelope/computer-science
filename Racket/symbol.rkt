#lang racket
'(a b c)
(symbol? 'a)
(symbol? '1)
(symbol? '(a b c))
(symbol? (string->symbol "a"))
(eq? 'a 'a)
#ci'A
#ci'abc

(string->symbol "one, two")
(string->symbol "one, two, three, four")
(string->symbol "12345")
