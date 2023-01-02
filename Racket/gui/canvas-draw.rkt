#lang racket

(require racket/gui)

(define (center-pos width height)
  (define-values (screen-width screen-height) (get-display-size))
  (values (/ (- screen-width width) 2)
          (/ (- screen-height height) 2)
          width
          height))

(define (login)
  (define-values (x y w h) (center-pos 300 100))
  (define frame (instantiate dialog% ("登录")
                  [x x]
                  [y y]
                  [min-width w]
                  [min-height h]))
  
  (define message-min-width 40)
  (define panel-username (new horizontal-panel%
                              [parent frame]
                              [alignment '(center center)]
                              [horiz-margin 10]
                              ))
  (define panel-password (new horizontal-panel%
                              [parent frame]
                              [alignment '(center center)]
                              [horiz-margin 10]))
  (new message% [label "用户名"] [parent panel-username] [min-width message-min-width])
  (define username-text (new text-field% [label ""]
                             [parent panel-username]
                             [style '(single)]
                             [callback (lambda (btn e)
                                         (void))]
                             [stretchable-width #t]))
  (new message% [label "密码"] [parent panel-password] [min-width message-min-width])
  (define password-text (new text-field% [label ""]
                             [parent panel-password]
                             [style (list 'single 'password)]
                             [callback (lambda (btn e)
                                         (let ((psw (send btn get-value))
                                               (maxlen 10))
                                           (when (> (string-length psw) maxlen)
                                             (send btn set-value (substring psw 0 maxlen)))))]
                             ))
  ; Add a horizontal panel to the dialog, with centering for buttons
  (define panel (new horizontal-panel% [parent frame]
                     [alignment '(center center)]))
  (new button%
       [label "登录"]
       [parent panel]
       [callback (lambda (btn e)
                   (send username-text set-value (send password-text get-value)))])
  (new button%
       [label "取消"]
       [parent panel]
       [callback (lambda (btn e)
                   (send frame on-exit))])
  (send frame show #t)
  )

(define (main)
  (define-values (x y w h) (center-pos 400 300))
  (define frame (new frame%
                     [label "Example"]
                     [x x]
                     [y y]
                     [width w]
                     [height h]))
  (define canvas (new canvas% [parent frame]
                      [paint-callback
                       (lambda (canvas dc)
                         
                         (send dc set-scale 3 3)
                         (send dc set-text-foreground "blue")
                         (send dc draw-text "Don't Panic!" 0 0))]))
  (send frame show #t)
  )
;(login)
(main)
