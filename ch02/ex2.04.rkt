#lang racket

(define (cons x y)
    (lambda (m) (m x y)))
(define (car z)
    (z (lambda (p q) p)))
(define (cdr z)
    (z (lambda (p q) q)))

(module+ test
    (require rackunit)
    (check-eq? (car (cons 1 2)) 1)
    (check-eq? (cdr (cons 1 2)) 2))