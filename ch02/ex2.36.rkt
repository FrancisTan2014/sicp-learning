#lang racket

(require "sequence-operations.rkt")

(provide accumulate
         accumulate-n)

(define (accumulate-n op initial seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulate op initial (map car seqs))
              (accumulate-n op initial (map cdr seqs)))))
            
(module+ main
    (accumulate-n + 0 (list '(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12))))