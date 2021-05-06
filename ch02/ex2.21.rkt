; aims to practise the usage of map
; and to understan the abstraction barrier again

#lang racket

(require "../base/math.rkt")

(define (square-list items)
    (if (null? items)
        '()
        (cons (square (car items))
              (square-list (cdr items)))))

(define (square-list-map items)
    (map square items))

(module+ main
    (let ((l '(1 2 3 4)))
         (square-list l)
         (square-list-map l)))