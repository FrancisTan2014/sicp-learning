#lang racket

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

(fixed-point cos 1.0)
(fixed-point sin 1.0)
(fixed-point (lambda (x) (+ (sin x) (cos x)))
                            1.0)

(define (sqrt x)
    (define (average a b) 
        (/ (+ a b)
           2))
    (fixed-point (lambda (y) (average y (/ x y)))
                 1.0))
                
(sqrt 2)