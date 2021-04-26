#lang racket

(require "sequence-operations.rkt")

(define (count-leaves t)
    (accumulate +
                0
                (map (lambda (x) 1)
                     (enumerate-tree t))))

(module+ main
    (define t (cons (list 1 2) (list 3 4)))
    (count-leaves t))