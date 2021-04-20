#lang racket

; recursive
(define (accumlate-r combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumlate-r combiner
                               null-value
                                term
                                (next a)
                                next
                                b))))
; iterative
(define (accumlate-i combiner null-value term a next b)
    (define (iter n result)
        (if (> n b)
            result
            (iter (next n)
                  (combiner (term n) result))))
    (iter a null-value))

(define accumlate accumlate-i)

(define (sum term a next b)
    (accumlate + 0 term a next b))

(sum 
    (lambda (x) x)
    1
    (lambda (x) (+ x 1))
    100)

(define (product term a next b)
    (accumlate * 1 term a next b))

(product 
    (lambda (x) x)
    1
    (lambda (x) (+ x 1))
    5)