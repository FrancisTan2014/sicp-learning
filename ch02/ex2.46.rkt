#lang racket

(provide (all-defined-out))

(define (add-vect v1 v2)
    (make-vect (+ (xcor-vect v1) (xcor-vect v2))
               (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
    (make-vect (- (xcor-vect v1) (xcor-vect v2))
               (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
    (make-vect (* s (xcor-vect v))
               (* s (ycor-vect v))))

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (same-vect? v1 v2)
    (and (= (xcor-vect v1) (xcor-vect v2))
         (= (ycor-vect v1) (ycor-vect v2))))

(module+ test
    (require rackunit)
    (check-eq? (xcor-vect (make-vect 3 4)) 3)
    (check-eq? (ycor-vect (make-vect 3 4)) 4)
    (check-true (same-vect? (make-vect 1 2) (make-vect 1 2)))
    (let ([v1 (make-vect 3 4)]
          [v2 (make-vect 1 2)])
        (check-true (same-vect? (add-vect v1 v2)
                                (make-vect 4 6)))
        (check-true (same-vect? (sub-vect v1 v2)
                                (make-vect 2 2)))
        (check-true (same-vect? (scale-vect 2 v1)
                                (make-vect 6 8)))))