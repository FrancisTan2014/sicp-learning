#lang racket

; (define 1 'w)
; By doing this, we'll get an error 'bad syntax' which
; means that we cannot re-define the numeral symbols in Lisp.

; (define (+ a b) (* a b))
; (+ 3 4)
; print: 12
; By doing this we know that we can re-define built-in 
; symbols in Lisp.
