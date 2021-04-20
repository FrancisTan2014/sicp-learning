#lang racket

; Rewrite the sum procedure to make
; it performe iteratively.

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

(define size 100000000)

(define (measure p)
    (define start (current-inexact-milliseconds))
    (define result (p))
    (define end (current-inexact-milliseconds))
    (fprintf (current-output-port)
             "result: ~s ellapsed: ~sms\n"
             result
             (- end start)))

(measure (lambda () (sum-r identity 1 inc size)))
(measure (lambda () (sum-i identity 1 inc size)))