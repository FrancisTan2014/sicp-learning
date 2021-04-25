#lang racket

(require "../base/math.rkt")

(define (for-each action l)
    (define (iter l)
        (cond ((null? l) #t)
              (else (action (car l))
                    (iter (cdr l)))))
    (iter l))

(module+ main
    (for-each (lambda (i) 
                (newline)
                (display i))
              '(1 2 3 4)))