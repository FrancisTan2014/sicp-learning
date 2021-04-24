#lang racket

(define (last-pair l)
    (define (iter l result)
        (if (null? l)
            result
            (iter (cdr l) (car l))))
    (iter l nil))

(module+ test
    (require rackunit)
    (check-eq? (last-pair (list 1 2 3 4)) 4))