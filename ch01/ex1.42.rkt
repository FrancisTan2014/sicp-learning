#lang racket

(require "../base/math.rkt")

(define (compose f g)
    (lambda (x) (f (g x))))

(module+ main
    ((compose square inc) 6))