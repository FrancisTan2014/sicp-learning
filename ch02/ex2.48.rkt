#lang racket

(require "ex2.46.rkt")

(provide (all-defined-out))

(define (make-segment start end) (list start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (car (cdr segment)))
(define (mid-segment segment)
    (scale-vect (/ 1 2)
                (add-vect (start-segment segment)
                          (end-segment segment))))

(module+ test
    (require rackunit)
    (let ([start (make-vect 1 1)]
          [end (make-vect 3 3)]
          [mid (make-vect 2 2)])
        (let ([seg (make-segment start end)])
            (check-true (same-vect? (start-segment seg) start))
            (check-true (same-vect? (end-segment seg) end))
            (check-true (same-vect? (mid-segment seg) mid)))))