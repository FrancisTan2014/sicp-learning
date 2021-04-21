#lang racket

(require "../base/math.rkt")

(define (double f)
    (lambda (x) (f (f x))))

(module+ main
    (((double (double double)) inc) 5))

; expand from the inside (double double)
; that will be: 
;    (double (double x))
; we must clearly know that the x here
; is not a representation of a number
; to make it clear, change the expression
; to: 
;    (double (double f))
; then:
;    (double (double double))
;    (double (double (double (double f))))
; go on, take inc as f:
;    (double (double (double (double inc))))
;    (double (double (double (inc (inc x)))))
;    (double (double (inc (inc (inc (inc x))))))
; .....
; according to the derivings, the inc function
; will be invoked by 16 times, thus, we got 21