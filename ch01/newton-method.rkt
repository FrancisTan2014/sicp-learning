#lang racket

; fixed point definition
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

; newton's method deriving
(define dx .00001)
(define (deriv g)
    (lambda (x) 
        (/ (- (g (+ x dx))
              (g x))
           dx)))

(define cube (lambda (x) (* x x x)))
((deriv cube) 5)

(define (newton-transform g)
    (lambda (x)
        (- x
           (/ (g x)
              ((deriv g) x)))))

; newton's method definition
(define (newton-method g guess)
    (fixed-point (newton-transform g)
                 guess))

(define (sqrt x)
    (newton-method (lambda (y) (- (* y y) x))
                   1.0))

(sqrt 2)