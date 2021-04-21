#lang racket

(require "../base/math.rkt"
         "ex1.42.rkt")

(define (repeated f n)
    (define (iter i result)
        (if (> i n)
            result
            (compose f result)))
    (iter 1 f))

(module+ main
    ((repeated square 2) 5))