#lang racket

(require "ordered-set.rkt"
         "ex2.18.rkt")

(define (adjoin-set o set)
    (if (element-of-set? o set)
        set
        (reverse (cons o (reverse set)))))

(module+ main
    (adjoin-set 4 '(1 2 3)))