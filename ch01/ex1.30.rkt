#lang racket

; Rewrite the sum procedure to make
; it performe iteratively.

(require "../base/tools.rkt")

(define (sum-r term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum-r term (next a) next b))))

(define (sum-i term a next b)
    (define (iter n result)
        (if (> n b)
            result
            (iter (next n)
                  (+ (term n) result))))
    (iter a 0))

(define (identity x) x)
(define (square x) (* x x))
(define (inc x) (+ x 1))

(define size 10000000)

(measure (lambda () (sum-r identity 1 inc size)))
(measure (lambda () (sum-i identity 1 inc size)))