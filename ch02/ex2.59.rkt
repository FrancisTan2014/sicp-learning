#lang racket

(require "set-unordered.rkt")

(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((element-of-set? (car set1) set2)
            (union-set (cdr set1) set2))
          (else (cons (car set1)
                      (union-set (cdr set1) set2)))))

(module+ main
    (union-set '(1 2 3) '(2 3 4 5)))