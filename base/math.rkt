#lang racket

(provide (all-defined-out))

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (abs x) (if (< x 0) (- x) x))
(define (1+ x) (+ x 1))
(define inc 1+)
(define (-1+ x) (- x 1))
(define dec -1+)
; just for positive integers(both x and y)
(define (power-pos x y)
  (define (iter i result)
    (if (> i y)
        result
        (iter (+ i 1)
              (* x result))))
  (if (= y 0)
      1
      (iter 1 1)))

(module+ test
  (check-eq? (power-pos 3 0) 1)
  (check-eq? (power-pos 3 1) 3)
  (check-eq? (power-pos 3 2) 9)
  (check-eq? (power-pos 3 3) 27)
  (check-eq? (power-pos 3 4) 81)
  (check-eq? (power-pos 3 5) 243))

(module+ test
  (require rackunit)
  (check-eq? (square 2) 4)
  (check-eq? (cube 1) 1)
  (check-eq? (cube 2) 8)
  (check-eq? (abs -1) 1)
  (check-eq? (abs 1) 1))