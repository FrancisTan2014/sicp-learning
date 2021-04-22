#lang racket

(require rackunit)

(provide (all-defined-out))

(define (check-in-tolerance actural expected [tolerance 1e-5])
    (check-true (< (abs (- actural expected))
                   tolerance)))