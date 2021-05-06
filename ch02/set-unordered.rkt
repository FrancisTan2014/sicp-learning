#lang racket

(provide (all-defined-out))

(define (element-of-set? o set)
    (cond ((null? set) #f)
          ((equal? o (car set)) #t)
          (else (element-of-set? o (cdr set)))))

(define (adjoin-set o set)
    (if (element-of-set? o set)
        set
        (cons o set)))

(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2)
              (cons (car set1) (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))

(module+ main
    (element-of-set? 1 '(1 2 3))
    (adjoin-set 2 '(1 2 3))
    (adjoin-set 4 '(1 2 3))
    (intersection-set '(1 2 3 4) '(3 4 5 6)))