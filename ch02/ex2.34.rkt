#lang racket

(require "sequence-operations.rkt")

(define (horner-eval x cofficient-sequence)
    (accumulate (lambda (this-coeff high-terms) (+ (* high-terms x) this-coeff))
                0
                cofficient-sequence))

(module+ test
    (require rackunit)
    ; 1+3x+5x^3+x^5 at x = 2
    (check-eq? (horner-eval 2 '(1 3 0 5 0 1)) 79))