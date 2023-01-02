#lang racket

(require racket/gui)

(define (main)
  ; Make a frame by instantiating the frame% class
  (define frame (new frame%
                     [label "Example"]
                     [width 800]
                     [height 600]
                     [x 400]
                     [y 400]
                     [min-width 800]
                     [min-height 600]
                     [stretchable-width #f]
                     [stretchable-height #f]
                     ))
   
  ; Make a static text message in the frame
  (define msg (new message%
                   [parent frame]
                   [color (make-color 127 0 0)]
                   [label "No events so far..."]))
  
  ; Make a button in the frame
  (new button% [parent frame]
       [label "Click Me"]
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   (send msg set-label "Button click"))])

  (new button%
       [parent frame]
       [label "Color"]
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   (begin (send msg set-color (make-color (random 255)
                                                          (random 255)
                                                          (random 255)))
                          (send msg set-label "Color change")))])

  (define panel (new horizontal-panel% [parent frame]))

  (new button% [parent panel]
       [label "Left"]
       [callback (lambda (button event)
                   (send msg set-label "Left click"))])

  (new button% [parent panel]
       [label "Right"]
       [callback (lambda (button event)
                   (send msg set-label "Right click"))])
   
  ; Show the frame by calling its show method
  (send frame show #t)
  )
(main)
