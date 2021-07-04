#lang racket

#("a" "b" "c")
#(name (that tune))
#10(a b)
(vector-ref #("a" "b" "c") 1)
(list->vector (map string-titlecase
                   (vector->list #("hello" "world" "!"))))

