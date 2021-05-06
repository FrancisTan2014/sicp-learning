#lang racket

; To keep our discussion simple, 
; we will consider only the case
; where the set elements are numbers.

(provide (all-defined-out))

(define (element-of-set? o set)
    (cond ((null? set) #f)
          ((= o (car set)) #t)
          ((< o (car set)) #f)
          (else (element-of-set? o (cdr set)))))

(define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ([x1 (car set1)]
              [x2 (car set2)])
            (cond ((= x1 x2)
                    (cons x1 (intersection-set (cdr set1) (cdr set2))))
                  ((< x1 x2)
                    (intersection-set (cdr set1) set2))
                  ((< x2 x1)
                    (intersection-set set1 (cdr set2)))))))

(module+ main
    (element-of-set? 3 '(1 2 3 4))
    (intersection-set '(1 2 3 4) '(3 4 5 6)))