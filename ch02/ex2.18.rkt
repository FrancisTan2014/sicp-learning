#lang racket

(provide (all-defined-out))

(define (reverse l)
    (define (iter rest result)
        (if (null? rest)
            result
            (iter (cdr rest) 
                  (cons (car rest) result))))
    (iter l '()))

(module+ main
    (display (reverse (list 1 2 3 4))))