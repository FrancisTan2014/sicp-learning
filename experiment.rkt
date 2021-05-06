#lang racket

; (define 1 'w)
; By doing this, we'll get an error 'bad syntax' which
; means that we cannot re-define the numeral symbols in Lisp.

; (define (+ a b) (* a b))
; (+ 3 4)
; print: 12
; By doing this we know that we can re-define built-in 
; symbols in Lisp.

; Graphics
(require graphics/graphics)

(module+ main
    (open-graphics)
    (let ([vp (open-viewport "main" 800 600)])
        ((draw-line vp) (make-posn 1 1)
                        (make-posn 100 100)
                        "black")
        (let ([dl draw-line])
            (define (draw-line v1 v2)
                ((dl vp) (make-posn (car v1) (cdr v1))
                         (make-posn (car v2) (cdr v2))
                         "red"))
            (draw-line (cons 70 80) (cons 100 100)))
    )
    )
