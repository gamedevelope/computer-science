#lang racket

(require net/rfc6455)
(ws-serve* #:port 8081
           (ws-service-mapper
            ["/test" ; the URL path (regular expression)
             [(subprotocol) ; if client requests subprotocol "subprotocol"
              (lambda (c) (ws-send! c "You requested a subprotocol"))]
             [(#f) ; if client did not request any subprotocol
              (lambda (c) (ws-send! c "You didn't explicitly request a subprotocol"))]]))
