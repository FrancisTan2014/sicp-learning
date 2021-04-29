#lang racket

(require "../base/math.rkt")

(provide (all-defined-out))

(define (for-each action l)
    (define (iter l)
        (cond ((not (null? l))
                (action (car l))
                (iter (cdr l)))))
    (iter l))

(module+ main
    (for-each (lambda (i) 
                (newline)
                (display i))
              '(1 2 3 4)))