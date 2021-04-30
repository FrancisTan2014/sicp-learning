#lang racket

; only symbols are considered here
(define (equal? l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((and (not (pair? l1))
                (not (pair? l2)))
           (eq? l1 l2))
          ((or (and (pair? l1) (not (pair? l2)))
               (and (pair? l2) (not (pair? l1))))
           #f)
          (else (if (eq? (car l1) (car l2))
                    (equal? (cdr l1) (cdr l2))
                    #f))))

(module+ test
    (require rackunit)
    (check-true (equal? '() '()))
    (check-true (equal? 'a 'a))
    (check-false (equal? 'a 2))
    (check-true (equal? '(this is a list) '(this is a list)))
    (check-false (equal? '(this is a list) '(this (is a) list))
    (let ([a 2])
        (check-true (equal? a 2))))
    )