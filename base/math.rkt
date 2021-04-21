#lang racket

(provide (all-defined-out))

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (abs x) (if (< x 0) (- x) x))
(define (1+ x) (+ x 1))
(define inc 1+)
(define (-1+ x) (- x 1))
(define dec -1+)

(module+ test
  (require rackunit)
  (check-eq? (square 2) 4)
  (check-eq? (cube 1) 1)
  (check-eq? (cube 2) 8)
  (check-eq? (abs -1) 1)
  (check-eq? (abs 1) 1))