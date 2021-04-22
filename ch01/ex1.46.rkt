#lang racket

(define (average-damp f)
    (define (average x y)
        (/ (+ x y) 2))
    (lambda (x)
        (average x (f x))))

(define (iterative-improve good-enough? improve)
    (define (iter result)
        (if (good-enough? result)
            result
            (iter (improve result))))
    (lambda (guess) (iter guess)))

(define tolerance 1e-6)
; rewrite sqrt
(define (sqrt x)
    ((iterative-improve (lambda (y) (< (abs (- x (* y y)))
                                       tolerance))
                        (lambda (guess) ((average-damp (lambda (y) (/ x y)))
                                         guess)))
    1.0))

(module+ main
    (sqrt 2.0))


(define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
(define (iterative-improve-2 good-enough? improve)
    (define (iter guess)
        (let ((next (improve guess)))
            (if (good-enough? guess next)
                guess
                (iter next))))
    (lambda (guess) (iter guess)))

(define (fixed-point f [first-guess 1.0])
    ((iterative-improve-2 close-enough? f)
     first-guess))
    
(module+ main
    (fixed-point sin))