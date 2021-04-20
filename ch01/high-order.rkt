#lang racket

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))

(define (inc x) (+ x 1))
(define (cube x) (* x x x))

(define (sum-of-cube a b)
    (sum cube a inc b))

(sum-of-cube 1 10)

(define (sum-of-series a b)
    (define (identity x) x)
    (sum identity a inc b))

(sum-of-series 1 100)

(define (pi-sum a b)
    (define (pi-term x)
        (/ 1.0 (* x (+ x 2))))
    (define (pi-next x)
        (+ x 4))
    (sum pi-term a pi-next b))

(* 8 (pi-sum 1 100000000))