#lang racket

; (define (fib n)
;     (define (iter a b counter)
;         (if (= counter 1)
;             sum
;             (iter (+ a b)
;                   sum
;                   (+ b 1))))
;     (iter 1 0 1))

(provide fib)

(define (fib n)
    (fib-iter 1 0 n))

(define (fib-iter a b counter)
    (if (= counter 0)
        b
        (fib-iter (+ a b) a (- counter 1))))