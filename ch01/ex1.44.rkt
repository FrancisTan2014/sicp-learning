#lang racket

(require "ex1.43.rkt")

(provide smooth
         smooth-n)

(define dx .00001)
(define (average x y z)
    (/ (+ x y z)
       3))
(define (smooth f)
    (lambda (x) 
        (average (f (- x dx))
                 (f x)
                 (f (+ x dx)))))
(define (smooth-n f n)
    ((repeated smooth n) f))

(module+ main
    ((smooth-n (lambda (x) (+ (* 3 (* x x))
                              (* 4 x)
                              5))
               5)
     3))