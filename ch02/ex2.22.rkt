#lang racket

(require "../base/math.rkt")

(define (square-list-1 items) 
    (define (iter things answer) 
        (if (null? things)
            answer
            (iter (cdr things)
                  (cons (square (car things))
                        answer))))
    (iter items '()))

(define (square-list-2 items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                  (cons answer
                        (square (car things))))))
    (iter items '()))

(module+ main
    (square-list-2 '(1 2 3 4)))