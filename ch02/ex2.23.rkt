#lang racket

(provide (all-defined-out))

(define (for-each action l)
    (define (iter rest)
        (cond ((not (null? rest))
                (action (car rest))
                (iter (cdr rest)))))
    (iter l))

(module+ main
    (for-each (lambda (i) 
                (newline)
                (display i))
              '(1 2 3 4)))