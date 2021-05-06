#lang racket

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
; (1 2 3 4 5 6)

(cons x y)
; ((1 2 3) 4 5 6)
; because (cons a (list b c d))
; equals to (list a b c d)

(list x y)
; ((1 2 3) (4 5 6))