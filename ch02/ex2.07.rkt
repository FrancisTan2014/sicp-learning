#lang racket

(provide (all-defined-out))

(define (make-interval a b) (cons a b))
(define (upper-bound i) (max (car i) (cdr i)))
(define (lower-bound i) (min (car i) (cdr i)))

(module+ test
    (require rackunit)
    (let ((i (make-interval 6 7)))
        (check-eq? (lower-bound i) 6)
        (check-eq? (upper-bound i) 7)))