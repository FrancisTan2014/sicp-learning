#lang racket

(require "../base/math.rkt")
(require "newton-method.rkt")

(define (cubic a b c)
    (lambda (x) (+ (cube x)
                   (* a (square x))
                   (* b x)
                   c)))

(module+ main
    ((cubic 3 5 2) 10)
    (newton-method (cubic 3 5 2) 1))