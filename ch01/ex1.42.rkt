#lang racket

(require "../base/math.rkt")

(provide compose)

(define (compose f g)
    (lambda (x) (f (g x))))

(module+ main
    ((compose square inc) 6))