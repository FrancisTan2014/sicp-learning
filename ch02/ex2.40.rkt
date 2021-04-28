#lang racket

(require 
    "nesting-mappings.rkt"
    "sequence-operations.rkt")

(define (unique-pairs n)
    (flatmap
        (lambda (i)
            (map (lambda (j) (cons i j))
                 (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))

(define (prime-sum-pairs-1 n)
    (map make-pair-sum
         (filter prime-sum?
                 (unique-pairs n))))

(module+ main
    (unique-pairs 6)
    (prime-sum-pairs-1 6))