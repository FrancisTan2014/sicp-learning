#lang racket

(provide make-frame
         origin-frame
         edge1-frame
         edge2-frame)

(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (car (cdr frame)))
(define (edge2-frame frame) (car (cddr frame)))

(define (make-frame-2 origin edge1 edge2)
    (cons origin (cons edge1 edge2)))
(define (origin-frame-2 frame) (car frame))
(define (edge1-frame-2 frame) (car (cdr frame)))
(define (edge2-frame-2 frame) (cdr (cdr frame)))

(module+ test
    (require rackunit)
    (require "ex2.46.rkt")
    (let ([o (make-vect 1 2)]
          [e1 (make-vect 3 4)]
          [e2 (make-vect 0 7)])
        (let ([f (make-frame o e1 e2)])
            (check-true (same-vect? (origin-frame f) o))
            (check-true (same-vect? (edge1-frame f) e1))
            (check-true (same-vect? (edge2-frame f) e2)))))
