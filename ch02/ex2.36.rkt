#lang racket

(require "sequence-operations.rkt")

(define (accumulate-n op initial seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulate + 0 (map car seqs))
              (accumulate-n + 0 (map cdr seqs)))))
            
(module+ main
    (accumulate-n + 0 (list '(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12))))