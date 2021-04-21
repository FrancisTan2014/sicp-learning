#lang racket

(provide fixed-point)

(define tolerance .00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2))
           tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                guess
                (try next))))
    (try first-guess))

(module+ main
    (fixed-point cos 1.0)
    (fixed-point sin 1.0)
    (fixed-point (lambda (x) (+ (sin x) (cos x)))
                                1.0))

(define (average a b) 
    (/ (+ a b)
       2))
(define (average-damp f)
    (lambda (x)
        (average x (f x))))
(define (sqrt x)
    (fixed-point (average-damp (lambda (y) (/ x y)))
                 1.0))
                


(define (cube-root x)
    (fixed-point (average-damp (lambda (y) (/ x (* y y))))
                 1.0))

(module+ main
    (sqrt 2)
    (cube-root 3))