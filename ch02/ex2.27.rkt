#lang racket

(require "ex2.18.rkt")

(provide (all-defined-out))

(define (deep-reverse l)
    (define (iter rest result)
        (if (null? rest)
            result
            (iter (cdr rest)
                  (cons (if (pair? (car rest))
                            (iter (car rest) '())
                            (car rest))
                        result))))
    (iter l '()))

(module+ main
    (deep-reverse (list (list 1 2) (list 3 4))))
    (deep-reverse '(1 (2 (3 (4 (5 (6 7)))))))