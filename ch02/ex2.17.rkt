#lang racket

(define (last-pair l)
    (define (iter l result)
        (if (null? l)
            result
            (iter (cdr l) (car l))))
    (iter l nil))

